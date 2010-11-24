Mod: adt {
	pick {
	Ins or
	Del =>
		# o & p are for start of change
		o: int;
		p: Pos;
		s: string;
	# Repl
	# del: ref Mod.Del;
	# ins: ref Mod.Ins;
	}

	invert:	fn(m: self ref Mod): ref Mod;
	text:	fn(m: self ref Mod): string;
};

Change: adt {
	begin,
	end:	Pos;
	l:	list of ref Mod;  # hd of list is change at end of file

	invert:	fn(c: self ref Change): ref Change;
	text:	fn(c: self ref Change): string;
};


Mod.invert(mm: self ref Mod): ref Mod
{
	pick m := mm {
	Ins =>	return ref Mod.Del (m.o, m.p, m.s);
	Del =>	return ref Mod.Ins (m.o, m.p, m.s);
	}
}

modtags := array[] of {"Ins", "Del"};
Mod.text(mm: self ref Mod): string
{
	s := sprint("Mod.%s(", modtags[tagof mm]);
	pick m := mm {
	Ins =>	s += sprint("o=%d, p=%s, s=%q", m.o, m.p.text(), m.s);
	Del =>	s += sprint("o=%d, p=%s, s=%q", m.o, m.p.text(), m.s);
	}
	s += ")";
	return s;
}

Change.invert(cc: self ref Change): ref Change
{
	c := ref Change (cc.end, cc.begin, nil);
	for(l := rev(cc.l); l != nil; l = tl l)
		c.l = (hd l).invert()::c.l;
	return c;
}

Change.text(c: self ref Change): string
{
	s := sprint("Change(begin=%s, end=%s, ", c.begin.text(), c.end.text());
	for(l := c.l; l != nil; l = tl l)
		s += (hd l).text()+"; ";
	s += ")";
	return s;
}
