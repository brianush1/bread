module bread.ir;
import bread.analysis;
public import bread.enums : BinaryOp;
import bread.source;

abstract class Node {}

final class Program : Node {
	Decl[] body;
}

abstract class Stat : Node {}

final class Decl : Stat {
	string name;
	Expr initValue;
	string ns;
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
	string ns;
}

final class True : Expr {}

final class False : Expr {}

final class Print : Expr {}

final class Nil : Expr {}

final class Binary : Expr {
	BinaryOp op;

	Expr lhs;
	Type lhsType;

	Expr rhs;
	Type rhsType;
}

final class Call : Expr {
	Expr func;
	Type funcType;

	Expr[] args;
	Type[] argTypes;
}

final class Function : Expr {
	string[] params;
	Stat[] body;
}
