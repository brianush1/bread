// compiled Bread code

function brstd$print(arg) {
	console.log(arg);
}

let br$rC91wA = true;
let br$Q_vDZw = false;
let br$CdMV8g = brstd$print;
let br$JFdG2A = function(br$WIw6wQ) {
	if ((br$WIw6wQ <= 1)) {
		return br$WIw6wQ;
	} else {}
	return (br$JFdG2A((br$WIw6wQ - 1 | 0)) + br$JFdG2A((br$WIw6wQ - 2 | 0)) | 0);
};
let br$5idXrg = function() {
	br$CdMV8g(5);
	return br$CdMV8g(3);
};
let br$WFGlGQ = function() {
	br$CdMV8g(br$JFdG2A(17));
	br$5idXrg();
};
br$WFGlGQ();