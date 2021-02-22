module bread.backend.js;
import bread.analysis;
import bread.ir;
import arsd.mvd;
import std.algorithm;
import std.conv;

private immutable(string) preamble = q"(// compiled Bread code

function brstd$binary(op, lhs, rhs) {
	// TODO: implement this
	throw 0;
}

function brstd$call(func, args) {
	// TODO: implement this
	throw 0;
}

function brstd$print(arg) {
	console.log(arg);
}
)";

private immutable(string) postamble = q"(
br$main();
)";

private string mangle(string name, string ns = "") {
	return "br" ~ ns ~ "$" ~ name;
}

private string indent(string s) {
	return s.splitter("\n").map!(x => "\t" ~ x).joiner("\n").to!string;
}

private string compileBlock(Stat[] stats) {
	if (stats.length == 0)
		return "{}";

	string result;
	foreach (stat; stats) {
		result ~= compile(stat) ~ "\n";
	}
	return "{\n" ~ indent(result)[0 .. $ - 1] ~ "}";
}

string compile(Node node) {
	return mvd!_compile(node);
}

string _compile(Program stat) {
	string[] result;
	result ~= preamble;
	foreach (v; stat.body) {
		result ~= compile(v);
	}
	result ~= postamble;
	return result.joiner("\n").to!string;
}

string _compile(Decl stat) {
	return "let " ~ stat.name.mangle(stat.ns) ~ " = " ~ stat.initValue.compile ~ ";";
}

string _compile(Return expr) {
	return "return " ~ expr.value.compile ~ ";";
}

string _compile(If expr) {
	return "if (" ~ expr.cond.compile ~ ") " ~ compileBlock(expr.body) ~ " else "
		~ compileBlock(expr.elseBody);
}

string _compile(ExprStat expr) {
	return compile(expr.value) ~ ";";
}

string _compile(Int expr) {
	return expr.value.to!string;
}

string _compile(VarAccess expr) {
	return expr.name.mangle(expr.ns);
}

string _compile(True expr) {
	return "true";
}

string _compile(False expr) {
	return "false";
}

string _compile(Print expr) {
	return "brstd$print";
}

string _compile(Nil expr) {
	return "undefined";
}

string _compile(Binary expr) {
	string lhs = compile(expr.lhs);
	string rhs = compile(expr.rhs);
	if (expr.lhsType is IntType.instance && expr.rhsType is IntType.instance) {
		switch (expr.op) {
			// arithmetic
			case BinaryOp.Add: return "(" ~ lhs ~ " + " ~ rhs ~ " | 0)";
			case BinaryOp.Sub: return "(" ~ lhs ~ " - " ~ rhs ~ " | 0)";
			case BinaryOp.Mul: return "Math.imul(" ~ lhs ~ ", " ~ rhs ~ ")";
			case BinaryOp.Div: return "(" ~ lhs ~ " / " ~ rhs ~ " | 0)";
			case BinaryOp.Mod: return "(" ~ lhs ~ " % " ~ rhs ~ ")"; // TODO: make sure this doesn't need "| 0"

			// relational
			case BinaryOp.Eq: return "(" ~ lhs ~ " === " ~ rhs ~ ")";
			case BinaryOp.Neq: return "(" ~ lhs ~ " !== " ~ rhs ~ ")";
			case BinaryOp.Lt: return "(" ~ lhs ~ " < " ~ rhs ~ ")";
			case BinaryOp.Gt: return "(" ~ lhs ~ " > " ~ rhs ~ ")";
			case BinaryOp.Le: return "(" ~ lhs ~ " <= " ~ rhs ~ ")";
			case BinaryOp.Ge: return "(" ~ lhs ~ " >= " ~ rhs ~ ")";

			default: assert(0);
		}
	}
	else {
		return "brstd$binary(" ~ (cast(int) expr.op).to!string ~ ", " ~ lhs ~ ", " ~ rhs ~ ")";
	}
}

string _compile(Call expr) {
	string func = compile(expr.func);
	string args = expr.args.map!(x => compile(x)).joiner(", ").to!string;
	if (cast(FunctionType) expr.funcType) {
		return func ~ "(" ~ args ~ ")";
	}
	else {
		return "brstd$call(" ~ func ~ ", [" ~ args ~ "])";
	}
}

string _compile(Function expr) {
	return "function(" ~ expr.params.map!mangle.joiner(", ").to!string ~ ") "
		~ compileBlock(expr.body);
}
