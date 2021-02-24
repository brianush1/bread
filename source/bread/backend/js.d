module bread.backend.js;
import bread.ir;
import arsd.mvd;
import std.algorithm;
import std.conv;
import std.random;

private immutable(string) preamble = q"(// compiled Bread code

function brstd$print(arg) {
	console.log(arg);
}
)";

private alias ID = ubyte[4];

private static Random rand = Random(0);

private ID nextId() {
	import std.array : array;
	import std.range : iota;

	static bool[ID] seen;

	ID result;
	do {
		result = iota(0, 4).map!(_ => cast(ubyte) uniform!"[]"(0, 255, rand)).array.to!ID;
	} while (result in seen);

	seen[result] = true;
	return result;
}

private string mangle(ID id) {
	import std.base64 : Base64Impl, Base64;

	return "br$" ~ Base64Impl!('$', '_', Base64.NoPadding).encode(id).to!string;
}

private static ID[string] ids;

private ID id(T)(T obj) {
	string name;
	static if (is(T == string)) {
		name = obj;
	}
	else {
		name = obj.name;
	}

	if (name !in ids) {
		ids[name] = nextId();
	}

	return ids[name];
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
	result ~= "main".id.mangle ~ "();";
	return result.joiner("\n").to!string;
}

string _compile(Decl stat) {
	return "let " ~ stat.id.mangle ~ " = " ~ stat.initValue.compile ~ ";";
}

string _compile(Native stat) {
	string result;
	foreach (i, v; stat.values) {
		if (stat.isVar[i]) {
			result ~= v.id.mangle;
		}
		else {
			result ~= v;
		}
	}
	result ~= ";";
	return result;
}

string _compile(Return stat) {
	return "return " ~ stat.value.compile ~ ";";
}

string _compile(If stat) {
	return "if (" ~ stat.cond.compile ~ ") " ~ compileBlock(stat.body) ~ " else "
		~ compileBlock(stat.elseBody);
}

string _compile(ExprStat stat) {
	return compile(stat.value) ~ ";";
}

string _compile(Int expr) {
	return expr.value.to!string;
}

string _compile(VarAccess expr) {
	return expr.id.mangle;
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
	if (expr.lhsType == Type.Int && expr.rhsType == Type.Int) {
		switch (expr.op) {
			// arithmetic
			case BinaryOp.Add: return "(" ~ lhs ~ " + " ~ rhs ~ " | 0)";
			case BinaryOp.Sub: return "(" ~ lhs ~ " - " ~ rhs ~ " | 0)";
			case BinaryOp.Mul: return "Math.imul(" ~ lhs ~ ", " ~ rhs ~ ")";
			case BinaryOp.Div: return "(" ~ lhs ~ " / " ~ rhs ~ " | 0)";
			case BinaryOp.Mod: return "(" ~ lhs ~ " % " ~ rhs ~ " | 0)";

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
		assert(0);
	}
}

string _compile(Call expr) {
	string func = compile(expr.func);
	string args = expr.args.map!(x => compile(x)).joiner(", ").to!string;
	if (expr.funcType == Type.Function) {
		return func ~ "(" ~ args ~ ")";
	}
	else {
		assert(0);
	}
}

string _compile(Function expr) {
	return "function(" ~ expr.params.map!id.map!mangle.joiner(", ").to!string ~ ") "
		~ compileBlock(expr.body);
}
