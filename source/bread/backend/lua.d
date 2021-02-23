module bread.backend.lua;
import bread.ir;
import arsd.mvd;
import std.algorithm;
import std.conv;
import std.random;

private immutable(string) preamble = q"(-- compiled Bread code

local function brstd_print(arg)
	print(arg)
end

local function brstd_int(x)
	local value = math.floor(math.abs(x))
	if x < 0 then
		value = -value
	end
	value = value % 4294967296
	if value > 2147483647 then
		return value - 4294967296
	else
		return value
	end
end
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
	import std.regex : ctRegex, replaceAll;

	return "br_" ~ Base64Impl!('$', '_', Base64.NoPadding).encode(id)
		.to!string
		.replaceAll(ctRegex!r"_", "_u_")
		.replaceAll(ctRegex!r"\$", "_s_");
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
		return " ";

	string result;
	foreach (stat; stats) {
		result ~= compile(stat) ~ "\n";
	}
	return "\n" ~ indent(result)[0 .. $ - 1];
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
	return "local " ~ stat.id.mangle ~ "; " ~ stat.id.mangle ~ " = " ~ stat.initValue.compile ~ ";";
}

string _compile(Return expr) {
	return "return " ~ expr.value.compile ~ ";";
}

string _compile(If expr) {
	return "if " ~ expr.cond.compile ~ " then" ~ compileBlock(expr.body) ~ "else"
		~ compileBlock(expr.elseBody) ~ "end";
}

string _compile(ExprStat expr) {
	return "local _ = " ~ compile(expr.value) ~ ";";
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
	return "brstd_print";
}

string _compile(Nil expr) {
	return "nil";
}

string _compile(Binary expr) {
	string lhs = compile(expr.lhs);
	string rhs = compile(expr.rhs);
	if (expr.lhsType == Type.Int && expr.rhsType == Type.Int) {
		switch (expr.op) {
			// arithmetic
			case BinaryOp.Add: return "brstd_int(" ~ lhs ~ " + " ~ rhs ~ ")";
			case BinaryOp.Sub: return "brstd_int(" ~ lhs ~ " - " ~ rhs ~ ")";
			case BinaryOp.Mul: return "brstd_int(" ~ lhs ~ " * " ~ rhs ~ ")";
			case BinaryOp.Div: return "brstd_int(" ~ lhs ~ " / " ~ rhs ~ ")";
			case BinaryOp.Mod: return "brstd_int(" ~ lhs ~ " % " ~ rhs ~ ")";

			// relational
			case BinaryOp.Eq: return "(" ~ lhs ~ " == " ~ rhs ~ ")";
			case BinaryOp.Neq: return "(" ~ lhs ~ " ~= " ~ rhs ~ ")";
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
	return "function(" ~ expr.params.map!id.map!mangle.joiner(", ").to!string ~ ")"
		~ compileBlock(expr.body) ~ "end";
}
