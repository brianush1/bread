module bread.ir;
public import bread.enums : BinaryOp;
import bread.source;
import std.uuid;

enum Type {
	Int,
	Bool,
	Function,
	Other,
}

abstract class Node {}

final class Program : Node {
	Decl[] body;
}

abstract class Stat : Node {}

final class Decl : Stat {
	string name;
	Expr initValue;
}

final class Native : Stat {
	bool[] isVar;
	string[] values;
}

final class Return : Stat {
	Expr value;
}

final class If : Stat {
	Expr cond;
	Stat[] body;
	Stat[] elseBody;
}

final class ExprStat : Stat {
	Expr value;
}

abstract class Expr : Node {}

final class Int : Expr {
	int value;
}

final class VarAccess : Expr {
	string name;
}

final class True : Expr {}

final class False : Expr {}

final class Print : Expr {}

final class Nil : Expr {}

final class Binary : Expr {
	BinaryOp op;
	Type lhsType;
	Type rhsType;

	Expr lhs;
	Expr rhs;
}

final class Call : Expr {
	Type funcType;
	Type[] argTypes;

	Expr func;
	Expr[] args;
}

final class Function : Expr {
	string[] params;
	Stat[] body;
}
