module bread.enums;

enum BinaryOp {
	// arithmetic
	Add = 0,
	Sub = 1,
	Mul = 2,
	Div = 3,
	Mod = 4,

	// relational
	Eq = 5,
	Neq = 6,
	Lt = 7,
	Gt = 8,
	Le = 9,
	Ge = 10,

	// boolean
	And = 11,
	Or = 12,
}
