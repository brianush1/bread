module bread.analysis;
import bread.source;
import bread.ast;
import std.algorithm;
import std.range;
import std.array;
import std.typecons;
import std.conv;
import sumtype;

private T expect(T, K)(K sumType) {
	return sumType.match!(
		delegate T(T value) {
			return value;
		},
		delegate T(_) {
			assert(0);
		},
	);
}

struct Value {
	alias Function = Value delegate(Value[]);

	alias Payload = SumType!(
		typeof(null),
		bool,
		int,
		Type,
		Function,
	);

	Payload payload;

	this(Args...)(Args args) {
		payload = Payload(args);
	}

	Value operate(Operation op, Value[] args) {
		final switch (op) {
		case Operation.Add:
			return payload.match!(
				(int value) {
					return Value(value + args[0].payload.expect!int);
				},
				delegate Value(_) {
					assert(0);
				},
			);
		case Operation.Mul:
			return payload.match!(
				(int value) {
					return Value(value * args[0].payload.expect!int);
				},
				delegate Value(_) {
					assert(0);
				},
			);
		case Operation.Call:
			return payload.match!(
				(Function value) {
					return value(args);
				},
				delegate Value(_) {
					assert(0);
				},
			);
		case Operation.TemplateCall:
			return payload.match!(
				(Function value) {
					return value(args);
				},
				delegate Value(_) {
					assert(0);
				},
			);
		}
	}
}

enum Operation {
	Add,
	Mul,
	Call,
	TemplateCall,
}

abstract class Type {
	/**

	Returns true if the type can accept all values of type `type`

	In other words, return true if all values of the given type are implicitly convertible to this type.

	*/
	abstract bool accepts(Type type);

	/**

	Return true if a value of this type can be used during runtime.

	*/
	abstract bool isRuntimeType();

	Type typeBinary(BinaryOp op, Type arg) {
		return null;
	}

	Type typeCall(Type[] args) {
		return null;
	}

	Type typeTemplateCall(Type[] argTypes, Value[] args) {
		return null;
	}

	override string toString() const {
		return "<type>";
	}
}

private mixin template Singleton() {
	private this() {}

	static typeof(this) instance;

	static this() {
		instance = new typeof(instance)();
	}
}

final class VoidType : Type {
	mixin Singleton;

	override bool accepts(Type type) {
		return type is this;
	}

	override bool isRuntimeType() {
		return true;
	}

	override string toString() const {
		return "void";
	}
}

final class BoolType : Type {
	mixin Singleton;

	override bool accepts(Type type) {
		return type is this;
	}

	override bool isRuntimeType() {
		return true;
	}

	override string toString() const {
		return "bool";
	}
}

final class IntType : Type {
	mixin Singleton;

	override bool accepts(Type type) {
		return type is this;
	}

	override bool isRuntimeType() {
		return true;
	}

	override Type typeBinary(BinaryOp op, Type arg) {
		if (arg is instance && [
			BinaryOp.Add,
			BinaryOp.Mul,
		].canFind(op)) {
			return instance;
		}

		return null;
	}

	override string toString() const {
		return "int";
	}
}

final class TypeType : Type {
	mixin Singleton;

	override bool accepts(Type type) {
		return type is this;
	}

	override bool isRuntimeType() {
		return false;
	}

	override string toString() const {
		return "type";
	}
}

final class FunctionType : Type {
	Type returnType;
	Type[] paramTypes;

	this(Type returnType, Type[] paramTypes) {
		this.returnType = returnType;
		this.paramTypes = paramTypes;
	}

	override bool accepts(Type type) {
		if (FunctionType other = cast(FunctionType) type) {
			if (!returnType.accepts(other.returnType)) {
				return false;
			}

			if (other.paramTypes.length > paramTypes.length) {
				return false;
			}

			foreach (i; 0 .. other.paramTypes.length) {
				if (!other.paramTypes[i].accepts(paramTypes[i])) {
					return false;
				}
			}

			return true;
		}
		else {
			return false;
		}
	}

	override bool isRuntimeType() {
		return returnType.isRuntimeType && paramTypes.all!(x => x.isRuntimeType);
	}

	override Type typeCall(Type[] args) {
		if (args.length != paramTypes.length)
			return null;

		foreach (i; 0 .. paramTypes.length) {
			if (!paramTypes[i].accepts(args[i])) {
				return null;
			}
		}

		return returnType;
	}

	override string toString() const {
		if (paramTypes.length == 0) {
			return "function(" ~ returnType.toString ~ ")";
		}
		return "function(" ~ returnType.toString ~ "; "
			~ paramTypes.map!(x => x.toString).joiner(", ").to!string ~ ")";
	}
}

private Type checkFunctionBody(Stat[] body, Environment env,
		void delegate() postStatic, bool isTemplate) {
	Type[] possibleReturns = checkBody(body, env, postStatic, isTemplate);
	if (possibleReturns.length == 0) {
		return VoidType.instance;
	}
	else {
		Type result = possibleReturns[0];
		foreach (v; possibleReturns[1 .. $]) {
			if (!result.accepts(v)) {
				assert(0); // TODO: union type instead
			}
		}
		return result;
	}
}

private Type[] checkBody(Stat[] body, Environment env,
		void delegate() postStatic, bool isTemplate) {
	Type[] possibleReturns;
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (decl.isStatic) {
				env.staticDecls[decl.name] = decl;
			}
		}
	}
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (decl.isStatic) {
				env.staticVar(decl.name);
			}
		}
	}
	postStatic();
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (!decl.isStatic) {
				Type typeType = env.check(decl.type, true);
				if (!TypeType.instance.accepts(typeType)) {
					addStack(decl.type.span);
					throw new AnalysisException("expected type, not value of type '"
						~ typeType.toString ~ "'");
				}
				Type type = env.eval(decl.type).payload.expect!Type;
				env.vars[decl.name] = Environment.Var(type);
				Type initType = env.check(decl.initValue, isTemplate);
				if (!type.accepts(initType)) {
					addStack(decl.initValue.span);
					throw new AnalysisException("value of type '"
						~ initType.toString ~ "' is not assignable to variable of type '"
						~ type.toString ~ "'");
				}
			}
		}
		else if (Return returnStat = cast(Return) stat) {
			possibleReturns ~= env.check(returnStat.value, isTemplate);
		}
		else if (ExprStat exprStat = cast(ExprStat) stat) {
			env.check(exprStat.value, isTemplate);
		}
		else if (If ifStat = cast(If) stat) {
			Type condType = env.check(ifStat.cond, isTemplate);
			if (!BoolType.instance.accepts(condType)) {
				addStack(ifStat.cond.span);
				throw new AnalysisException("value of type '"
					~ condType.toString ~ "' cannot be used as condition");
			}
			Environment thenEnv = new Environment;
			thenEnv.parent = env;
			possibleReturns ~= checkBody(ifStat.body, thenEnv, {}, isTemplate);

			Environment elseEnv = new Environment;
			elseEnv.parent = env;
			possibleReturns ~= checkBody(ifStat.elseBody, elseEnv, {}, isTemplate);
		}
	}
	return possibleReturns;
}

private Value evalFunctionBody(Stat[] body, Environment env, void delegate() postStatic) {
	Nullable!Value result = evalBody(body, env, postStatic);
	if (result.isNull) {
		return Value(null);
	}
	else {
		return result.get;
	}
}

private Nullable!Value evalBody(Stat[] body, Environment env, void delegate() postStatic) {
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (decl.isStatic) {
				env.staticDecls[decl.name] = decl;
			}
		}
	}
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (decl.isStatic) {
				env.staticVar(decl.name);
			}
		}
	}
	postStatic();
	foreach (stat; body) {
		if (Decl decl = cast(Decl) stat) {
			if (!decl.isStatic) {
				env.vars[decl.name] = Environment.Var(
					null,
					Nullable!Value(env.eval(decl.initValue)),
				);
			}
		}
		else if (Return returnStat = cast(Return) stat) {
			return Nullable!Value(env.eval(returnStat.value));
		}
		else if (ExprStat exprStat = cast(ExprStat) stat) {
			env.eval(exprStat.value);
		}
		else if (If ifStat = cast(If) stat) {
			Environment childEnv = new Environment;
			childEnv.parent = env;
			Nullable!Value result;
			if (env.eval(ifStat.cond).payload.expect!bool) {
				result = evalBody(ifStat.body, childEnv, {});
			}
			else {
				result = evalBody(ifStat.elseBody, childEnv, {});
			}
			if (!result.isNull) {
				return result;
			}
		}
	}
	return Nullable!Value.init;
}

final class TemplateType : Type {
	Type[] paramTypes;
	string idName;

	TemplateExpr expr;
	Environment env;

	this(string idName, Type[] paramTypes,
			TemplateExpr expr, Environment env) {
		this.idName = idName;
		this.paramTypes = paramTypes;
		this.expr = expr;
		this.env = env;
	}

	override bool accepts(Type type) {
		return this is type;
	}

	override bool isRuntimeType() {
		return false;
	}

	override Type typeTemplateCall(Type[] argTypes, Value[] args) {
		if (argTypes.length != paramTypes.length)
			return null;

		foreach (i; 0 .. paramTypes.length) {
			if (!paramTypes[i].accepts(argTypes[i])) {
				return null;
			}
		}

		Environment funcInner = new Environment;
		funcInner.parent = env;
		foreach (i, param; expr.params) {
			funcInner.staticVars[param.name] = Environment.StaticVar(paramTypes[i], args[i]);
		}
		return checkFunctionBody(expr.body, funcInner, {}, true);
	}

	override string toString() const {
		import std.format : format;

		return "template " ~ idName ~ "("
			~ paramTypes.map!(x => x.toString).joiner(", ").to!string ~ ")";
	}
}

class AnalysisException : Exception {

	Diagnostic diagnostic;

	this(string message) {
		super("An error has occurred during semantic analysis");
		diagnostic = Diagnostic(
			Diagnostic.Kind.Error,
			stack.length == 0 ? Nullable!Span.init : Nullable!Span(stack[$ - 1]),
			message,
			// "",
			// stack.length <= 1 ? [] : stack[0 .. $ - 1]
			// 		.retro.enumerate(1).map!(x => Diagnostic.Extra(
			// 	Diagnostic.Extra.Kind.Note,
			// 	"level " ~ x[0].to!string,
			// 	Nullable!Span(x[1]),
			// )).array,
		);
	}

}

Span[] stack;

void addStack(Span span) {
	stack.assumeSafeAppend ~= span;
}

void popStack() {
	stack = stack[0 .. $ - 1];
}

Environment analyze(Program program) {
	Environment result = new Environment;
	foreach (decl; program.decls) {
		result.staticDecls[decl.name] = decl;
	}
	foreach (decl; program.decls) {
		result.staticVar(decl.name);
	}
	return result;
}

final class Environment {

	struct StaticVar {
		Type type;
		Value value;
	}

	struct Var {
		Type type;

		Nullable!Value value;
	}

	StaticVar[string] staticVars;
	Decl[string] staticDecls;
	bool[string] staticVisited;

	Environment parent;

	Var[string] vars;

	StaticVar staticVar(string name) {
		if (auto resultPtr = name in staticVars) {
			return *resultPtr;
		}

		if (name !in staticDecls) {
			throw new AnalysisException("static variable '" ~ name ~ "' is not available");
		}

		if (name in staticVisited) {
			throw new AnalysisException("cyclic reference");
		}

		staticVisited[name] = true;

		Decl decl = staticDecls[name];
		addStack(decl.span);
		scope (exit)
			popStack();

		Value value = eval(decl.initValue);

		Type type;

		if (decl.type) {
			Type typeType = check(decl.type, true);
			if (!TypeType.instance.accepts(typeType)) {
				throw new AnalysisException("invalid type '" ~ typeType.toString ~ "'");
			}

			type = eval(decl.type).payload.expect!Type;

			Type realType = check(decl.initValue, true);
			if (!type.accepts(realType)) {
				throw new AnalysisException("'" ~ realType.toString
					~ "' is not assignable to '" ~ type.toString ~ "'");
			}
		}
		else {
			type = check(decl.initValue, true);
		}

		staticVars[name] = StaticVar(
			type,
			value,
		);
		return staticVars[name];
	}

	Type check(Expr expr_, bool staticEval) {
		addStack(expr_.span);
		scope (exit)
			popStack();

		if (VarAccess expr = cast(VarAccess) expr_) {
			Type result;
			if (expr.name in vars) {
				result = vars[expr.name].type;
			}
			else if (expr.name in staticDecls || expr.name in staticVars) {
				result = staticVar(expr.name).type;
			}
			else if (parent) {
				result = parent.check(expr, staticEval);
			}
			else {
				throw new AnalysisException("variable '" ~ expr.name ~ "' is not available");
			}
			if (TypeType.instance.accepts(result) && !staticEval) {
				throw new AnalysisException("can't use type in non-static evaluation context");
			}
			return result;
		}
		else if (BuiltinValue expr = cast(BuiltinValue) expr_) {
			if (expr.value == "int-type") {
				if (!staticEval) {
					throw new AnalysisException("can't create type in non-static evaluation context");
				}
				return TypeType.instance;
			}
			else if (expr.value == "void-type") {
				if (!staticEval) {
					throw new AnalysisException("can't create type in non-static evaluation context");
				}
				return TypeType.instance;
			}
			else if (expr.value == "bool-type") {
				if (!staticEval) {
					throw new AnalysisException("can't create type in non-static evaluation context");
				}
				return TypeType.instance;
			}
			else if (expr.value == "true") {
				return BoolType.instance;
			}
			else if (expr.value == "false") {
				return BoolType.instance;
			}
			else if (expr.value == "print") {
				return new FunctionType(VoidType.instance, [IntType.instance]);
			}
			else {
				throw new AnalysisException("unknown built-in '" ~ expr.value ~ "'");
			}
		}
		else if (TypeExpr expr = cast(TypeExpr) expr_) {
			if (!staticEval) {
				throw new AnalysisException("can't create type in non-static evaluation context");
			}
			return TypeType.instance;
		}
		else if (IntLiteral expr = cast(IntLiteral) expr_) {
			return IntType.instance;
		}
		else if (NilLiteral expr = cast(NilLiteral) expr_) {
			return VoidType.instance;
		}
		else if (BinaryExpr expr = cast(BinaryExpr) expr_) {
			Type lhsType = check(expr.lhs, staticEval);
			Type rhsType = check(expr.rhs, staticEval);
			Type result = lhsType.typeBinary(expr.op, rhsType);
			if (result is null) {
				throw new AnalysisException("operation '" ~ expr.op.to!string
					~ "' cannot be applied to types '"
					~ lhsType.toString ~ "' and '" ~ rhsType.toString ~ "'");
			}
			return result;
		}
		else if (CallExpr expr = cast(CallExpr) expr_) {
			Type funcType = check(expr.func, staticEval);
			Type[] argTypes = expr.args.map!(x => check(x, staticEval)).array;
			Type result = funcType.typeCall(argTypes);
			if (result is null) {
				throw new AnalysisException("type '" ~ funcType.toString
					~ "' cannot be called with arguments of types "
					~ argTypes.map!(x => "'" ~ x.toString ~ "'").joiner(", ").to!string
				);
			}
			return result;
		}
		else if (TemplateCallExpr expr = cast(TemplateCallExpr) expr_) {
			Type templateType = check(expr.templat, staticEval);
			Type[] argTypes = expr.args.map!(x => check(x, true)).array;
			Value[] args = expr.args.map!(x => eval(x)).array;
			Type result = templateType.typeTemplateCall(argTypes, args);
			if (result is null) {
				throw new AnalysisException("type '" ~ templateType.toString
					~ "' cannot be instantiated with arguments of types "
					~ argTypes.map!(x => "'" ~ x.toString ~ "'").joiner(", ").to!string
				);
			}
			return result;
		}
		else if (FunctionTypeExpr expr = cast(FunctionTypeExpr) expr_) {
			if (!staticEval) {
				throw new AnalysisException("can't create type in non-static evaluation context");
			}
			if (!TypeType.instance.accepts(check(expr.returnType, true))) {
				throw new AnalysisException("return type is invalid type");
			}
			foreach (i; 0 .. expr.paramTypes.length) {
				if (!TypeType.instance.accepts(check(expr.paramTypes[i], true))) {
					throw new AnalysisException("parameter " ~ i.to!string ~ " type is invalid type");
				}
			}
			return TypeType.instance;
		}
		else if (FuncExpr expr = cast(FuncExpr) expr_) {
			Type[] paramTypeTypes = expr.params.map!(x => check(x.type, true)).array;
			foreach (i; 0 .. paramTypeTypes.length) {
				if (!TypeType.instance.accepts(paramTypeTypes[i])) {
					throw new AnalysisException("parameter " ~ i.to!string ~ " type is invalid type");
				}
			}
			if (!TypeType.instance.accepts(check(expr.returnType, true))) {
				throw new AnalysisException("return type is invalid type");
			}
			Type returnType = eval(expr.returnType).payload.expect!Type;
			Type[] paramTypes = expr.params.map!(x => eval(x.type).payload.expect!Type).array;
			foreach (i; 0 .. paramTypes.length) {
				if (!paramTypes[i].isRuntimeType) {
					addStack(expr.params[i].type.span);
					throw new AnalysisException("cannot use type in non-static evaluation context");
				}
			}
			Environment funcInner = new Environment;
			funcInner.parent = this;
			Type realReturnType = checkFunctionBody(expr.body, funcInner, {
				foreach (i, param; expr.params) {
					funcInner.vars[param.name] = Var(paramTypes[i]);
				}
			}, false);
			if (!returnType.accepts(realReturnType)) {
				throw new AnalysisException("return type '" ~ realReturnType.toString
					~ "' is not assignable to '" ~ returnType.toString ~ "'");
			}
			return new FunctionType(returnType, paramTypes);
		}
		else if (TemplateExpr expr = cast(TemplateExpr) expr_) {
			Type[] paramTypeTypes = expr.params.map!(x => check(x.type, true)).array;
			foreach (i; 0 .. paramTypeTypes.length) {
				if (!TypeType.instance.accepts(paramTypeTypes[i])) {
					throw new AnalysisException("parameter " ~ i.to!string ~ " type is invalid type");
				}
			}
			Type[] paramTypes = expr.params.map!(x => eval(x.type).payload.expect!Type).array;
			return new TemplateType(expr.idName, paramTypes, expr, this);
		}
		else {
			assert(0, "dunno how to handle " ~ expr_.toString);
		}
	}

	/**

	Evaluates the value of an expression.
	Note that this does not do any type-checking.
	Expressions are assumed to be correctly formed when passed into this method.

	This will thus make no calls to $(REF check).

	*/
	Value eval(Expr expr_) {
		addStack(expr_.span);
		scope (exit)
			popStack();

		if (VarAccess expr = cast(VarAccess) expr_) {
			if (expr.name in vars && !vars[expr.name].value.isNull) {
				return vars[expr.name].value.get;
			}
			else if (expr.name in staticDecls || expr.name in staticVars) {
				return staticVar(expr.name).value;
			}
			else if (parent) {
				return parent.eval(expr);
			}
			else {
				throw new AnalysisException("variable '" ~ expr.name ~ "' is not available");
			}
		}
		else if (BuiltinValue expr = cast(BuiltinValue) expr_) {
			if (expr.value == "int-type") {
				return Value(cast(Type) IntType.instance);
			}
			else if (expr.value == "void-type") {
				return Value(cast(Type) VoidType.instance);
			}
			else if (expr.value == "bool-type") {
				return Value(cast(Type) BoolType.instance);
			}
			else if (expr.value == "true") {
				return Value(true);
			}
			else if (expr.value == "false") {
				return Value(false);
			}
			else if (expr.value == "print") {
				return Value(cast(Value.Function)(Value[] args) {
					import std.stdio : writeln;

					writeln(args[0].payload.expect!int);

					return Value(null);
				});
			}
			else {
				throw new AnalysisException("unknown built-in '" ~ expr.value ~ "'");
			}
		}
		else if (TypeExpr expr = cast(TypeExpr) expr_) {
			return Value(cast(Type) TypeType.instance);
		}
		else if (IntLiteral expr = cast(IntLiteral) expr_) {
			return Value(expr.value);
		}
		else if (NilLiteral expr = cast(NilLiteral) expr_) {
			return Value(null);
		}
		else if (BinaryExpr expr = cast(BinaryExpr) expr_) {
			Operation op;
			final switch (expr.op) {
			case BinaryOp.Add: op = Operation.Add; break;
			case BinaryOp.Mul: op = Operation.Mul; break;
			}
			return eval(expr.lhs).operate(op, [eval(expr.rhs)]);
		}
		else if (CallExpr expr = cast(CallExpr) expr_) {
			return eval(expr.func).operate(Operation.Call, expr.args.map!(x => eval(x)).array);
		}
		else if (TemplateCallExpr expr = cast(TemplateCallExpr) expr_) {
			return eval(expr.templat).operate(Operation.TemplateCall,
				expr.args.map!(x => eval(x)).array);
		}
		else if (FunctionTypeExpr expr = cast(FunctionTypeExpr) expr_) {
			Type returnType = eval(expr.returnType).payload.expect!Type;
			Type[] paramTypes = expr.paramTypes.map!(x => eval(x).payload.expect!Type).array;
			return Value(cast(Type) new FunctionType(returnType, paramTypes));
		}
		else if (FuncExpr expr = cast(FuncExpr) expr_) {
			return Value(cast(Value.Function)(Value[] args) {
				Environment funcInner = new Environment;
				funcInner.parent = this;
				return evalFunctionBody(expr.body, funcInner, {
					foreach (i, param; expr.params) {
						funcInner.vars[param.name] = Var(null, Nullable!Value(args[i]));
					}
				});
			});
		}
		else if (TemplateExpr expr = cast(TemplateExpr) expr_) {
			return Value(cast(Value.Function)(Value[] args) {
				Environment funcInner = new Environment;
				funcInner.parent = this;
				foreach (i, param; expr.params) {
					funcInner.staticVars[param.name] = StaticVar(
						eval(param.type).payload.expect!Type,
						args[i],
					);
				}
				return evalFunctionBody(expr.body, funcInner, {});
			});
		}
		else {
			assert(0, "dunno how to handle " ~ expr_.toString);
		}
	}

}
