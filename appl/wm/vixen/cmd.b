Cmd: adt {
	i:	int; # index of next char to return
	s:	string;
	n1,
	n2:	string;

	new:		fn(s: string): ref Cmd;
	clone:		fn(c: self ref Cmd): ref Cmd;
	char:		fn(c: self ref Cmd): int;
	get:		fn(c: self ref Cmd): int;
	unget:		fn(c: self ref Cmd);
	isnum:		fn(c: self ref Cmd): int;
	more:		fn(c: self ref Cmd): int;
	rem:		fn(c: self ref Cmd): string;
	num1:		fn(c: self ref Cmd, def: int): int;
	num2:		fn(c: self ref Cmd, def: int): int;
	getnum:		fn(c: self ref Cmd): string;
	getnum1:	fn(c: self ref Cmd);
	getnum2:	fn(c: self ref Cmd);
	text:		fn(c: self ref Cmd): string;
};

Cmd.new(s: string): ref Cmd
{
	return ref Cmd (0, s, "", "");
}

Cmd.clone(c: self ref Cmd): ref Cmd
{
	return ref *c;
}

Cmd.char(c: self ref Cmd): int
{
	if(c.i >= len c.s)
		return -1;
	return c.s[c.i];
}

Cmd.get(c: self ref Cmd): int
{
	if(c.i >= len c.s)
		return -1;
	return c.s[c.i++];
}


Cmd.unget(c: self ref Cmd)
{
	if(c.i <= 0)
		raise "unget at index <= 0";
	--c.i;
}

Cmd.isnum(c: self ref Cmd): int
{
	x := c.char();
	return x >= 0 && str->in(x, "1-9");
}

Cmd.more(c: self ref Cmd): int
{
	return c.i < len c.s;
}

Cmd.rem(c: self ref Cmd): string
{
	return c.s[c.i:];
}

Cmd.num1(c: self ref Cmd, def: int): int
{
	if(c.n1 == nil)
		return def;
	return int c.n1;
}

Cmd.num2(c: self ref Cmd, def: int): int
{
	if(c.n2 == nil)
		return def;
	return int c.n2;
}

Cmd.getnum(c: self ref Cmd): string
{
	s := "";
	if(c.isnum()) {
		s[len s] = c.get();
		while(c.more() && str->in(c.char(), "0-9"))
			s[len s] = c.get();
	}
	return s;
}

Cmd.getnum1(c: self ref Cmd)
{
	c.n1 = c.getnum();
}

Cmd.getnum2(c: self ref Cmd)
{

	c.n2 = c.getnum();
}

Cmd.text(c: self ref Cmd): string
{
	return sprint("Cmd(i=%d, s=%q, n1=%q, n2=%q)", c.i, c.s, c.n1, c.n2);
}

