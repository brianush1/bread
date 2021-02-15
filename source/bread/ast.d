module bread.ast;
import bread.source;
import std.typecons;
import std.algorithm;
import std.conv;

private string indent(string s) {
	return s.splitter("\n").map!(x => "    " ~ x).joiner("\n").to!string;
}

private mixin template prettyPrinter(T) {
	override string toString() const {
		import std.traits : FieldNameTuple, BaseClassesTuple;

		string result = T.stringof ~ " {\n";
		enum fields = {
			string[] fieldsResult;
			static foreach (field; FieldNameTuple!T) {
				fieldsResult ~= field;
			}
			static foreach (Base; BaseClassesTuple!T) {
				static foreach (field; FieldNameTuple!Base) {
					fieldsResult ~= field;
				}
			}
			return fieldsResult;
		}();
		static foreach (i, field; fields) {{
			static if (field != "span") {
				result ~= field ~ ": " ~ prettyPrint(__traits(getMember, this, field)) ~ ",\n";
			}
		}}
		return result.indent[4 .. $ - 4] ~ "}";
	}
}

string prettyPrint(T)(T value) {
	import std.traits : isSomeString;

	static if (is(T == class)) {
		if (value is null)
			return "null";
		return value.toString;
	}
	else static if (isSomeString!T) {
		return "\"" ~ value.to!string ~ "\"";
	}
	else static if (is(T == K[], K) && !isSomeString!T) {
		if (value == []) {
			return "[]";
		}
		static if (is(K == class) || is(K == struct)) {
			return "[\n" ~ value.map!(x => prettyPrint(x) ~ ",").joiner("\n").to!string.indent ~ "\n]";
		}
		else {
			return "[" ~ value.map!prettyPrint.joiner(", ").to!string ~ "]";
		}
	}
	else {
		return value.to!string;
	}
}

abstract class BaseNode {
	Span span;

	mixin prettyPrinter!(typeof(this));
}

final class Import : BaseNode {
	string[] path;

	mixin prettyPrinter!(typeof(this));
}

final class Program : BaseNode {
	Import[] imports;
	Decl[] decls;

	mixin prettyPrinter!(typeof(this));
}

abstract class Stat : BaseNode {
	mixin prettyPrinter!(typeof(this));
}

final class ExprStat : Stat {
	Expr value;

	mixin prettyPrinter!(typeof(this));
}

final class Return : Stat {
	Expr value;

	mixin prettyPrinter!(typeof(this));
}

final class Decl : Stat {
	bool isStatic;

	string name;

	/** may be null if static template; defined in all other cases */
	Expr type;

	Expr initValue;

	mixin prettyPrinter!(typeof(this));
}

abstract class Expr : BaseNode {
	mixin prettyPrinter!(typeof(this));
}

final class FuncExpr : Expr {
	struct Param {
		Expr type;
		string name;
	}

	Expr returnType;
	Param[] params;
	Stat[] body;

	mixin prettyPrinter!(typeof(this));
}

final class TemplateExpr : Expr {
	struct Param {
		Expr type;
		string name;
	}

	Param[] params;
	Stat[] body;

	string idName;

	mixin prettyPrinter!(typeof(this));
}

final class AddExpr : Expr {
	Expr lhs;
	Expr rhs;

	mixin prettyPrinter!(typeof(this));
}

final class CallExpr : Expr {
	Expr func;
	Expr[] args;

	mixin prettyPrinter!(typeof(this));
}

final class TemplateCallExpr : Expr {
	Expr templat;
	Expr[] args;

	mixin prettyPrinter!(typeof(this));
}

final class VarAccess : Expr {
	string name;

	mixin prettyPrinter!(typeof(this));
}

final class NilLiteral : Expr {
	mixin prettyPrinter!(typeof(this));
}

final class IntLiteral : Expr {
	int value;

	mixin prettyPrinter!(typeof(this));
}

final class BuiltinValue : Expr {
	string value;

	mixin prettyPrinter!(typeof(this));
}

final class TypeExpr : Expr {
	mixin prettyPrinter!(typeof(this));
}

final class FunctionTypeExpr : Expr {
	Expr returnType;
	Expr[] paramTypes;

	mixin prettyPrinter!(typeof(this));
}

// final class ConstExpr : Expr {
// 	Expr inner;

// 	mixin prettyPrinter!(typeof(this));
// }

// final class ImmutableExpr : Expr {
// 	Expr inner;

// 	mixin prettyPrinter!(typeof(this));
// }
