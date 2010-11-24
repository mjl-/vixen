implement Vixen;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "tk.m";
	tk: Tk;
include "tkclient.m";
	tkclient: Tkclient;
include "keyboard.m";
include "regex.m";
	regex: Regex;
include "plumbmsg.m";
	plumbmsg: Plumbmsg;
	Msg: import plumbmsg;
include "sh.m";
	sh: Sh;
include "util0.m";
	util: Util0;
	fail, warn, pid, kill, killgrp, min, max, l2a, rev: import util;

include "vixen/buffers.b";
include "vixen/change.b";
include "vixen/cmd.b";
include "vixen/filter.b";

Vixen: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};


dflag := 1;  # debug

Insert, Command0, Visual, Edit: con iota;  # modes
modes := array[] of {"insert", "command", "visual", "edit"};

Char, Line: con iota;
visualmodes := array[] of {"char", "line"};

Cnone, Cmod, Cnewchange, Cnewmod: con iota;  # where to record command in change?  none for undo/redo, mod for ^h

mode: int;
visualmode: int;  # only valid when mode == Visual
visualstart: ref Cursor;  # start of visual select, non-nil when mode == Visual
commandcur: string;  # current command
commandprev: string;  # previous (completed) command, for '.'

filename: string;  # may be nil initially
filefd: ref Sys->FD;  # may be nil initially
modified: int;  # whether text has unsaved changes
text: ref Buf;  # contents
cursor: ref Cursor;  # current position in text

statustext: string;  # info/warning/error text to display in status bar

searchregex: Regex->Re;  # current search
searchreverse: int;  # whether search is in reverse

edithist: list of string;  # history of edit commands, hd is last typed
edithistcur := -1;  # currently selected history item, -1 when none
edithisttext: string;  # text currently prefix-searching for (nil at start, after edit field changed, and esc)

change: ref Change;  # current change (with 1 modification) that is being created (while in insert mode)
changes: array of ref Change;  # change history, for undo.  first elem is oldest change.
changeindex: int;  # points to next new/last undone change.  may be one past end of 'changes'.

marks := array[128] of ref Pos;
registers := array[128] of string;
register: int;

plumbed: int;
top: ref Tk->Toplevel;
wmctl: chan of string;
drawcontext: ref Draw->Context;

tkcmds0 := array[] of {
"frame .m",
"text .m.text -background black -foreground white -selectbackground white -selectforeground black -yscrollcommand {.m.vscroll set}",
"scrollbar .m.vscroll -command {.m.text yview}",
"frame .s",
"label .s.status -text status",
"entry .edit",

"bind .edit <Key-\n> {send edit return}",
"bind .edit <KeyPress> +{send edit press %K}",
"bind .m.text <KeyPress> {send key %K}",
"bind .m.text <ButtonPress-1> {send text b1down}",
"bind .m.text <ButtonRelease-1> {send text b1up}",

".m.text tag configure eof -foreground blue -background white",

"pack .m.vscroll -fill y -side left",
"pack .m.text -fill both -expand 1 -side right",
"pack .m -fill both -expand 1",
"pack .s.status -fill x -side left",
"pack .s -fill x",
"pack .edit -fill x -side bottom",
"pack propagate . 0",
". configure -width 700 -height 500",
};


init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(ctxt == nil)
		fail("no window context");
	drawcontext = ctxt;
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	tk = load Tk Tk->PATH;
	tkclient = load Tkclient Tkclient->PATH;
	regex = load Regex Regex->PATH;
	plumbmsg = load Plumbmsg Plumbmsg->PATH;
	sh = load Sh Sh->PATH;
	sh->initialise();
	util = load Util0 Util0->PATH;
	util->init();

	arg->init(args);
	arg->setusage(arg->progname()+" [-d] [filename]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		* =>	arg->usage();
		}
	args = arg->argv();
	case len args {
	0 =>	;
	1 =>	filename = hd args;
	* =>	arg->usage();
	}

	sys->pctl(Sys->NEWPGRP, nil);
	plumbed = plumbmsg->init(1, nil, 0) >= 0;

	if(filename != nil) {
		filefd = sys->open(filename, Sys->ORDWR);
		# if filefd is nil, we warn that this is a new file when tk is initialized
	}
	text = text.new();
	cursor = text.pos(Pos(1, 0));

	tkclient->init();
	(top, wmctl) = tkclient->toplevel(ctxt, "", "vixen", Tkclient->Appl);

	textc := chan of string;
	keyc := chan of string;
	ctlc := chan of string;
	editc := chan of string;
	tk->namechan(top, textc, "text");
	tk->namechan(top, keyc, "key");
	tk->namechan(top, ctlc, "ctl");
	tk->namechan(top, editc, "edit");
	tkcmds(tkcmds0);
	tkcmd(sprint("bind .m.text <Key-%c> {send text esc}", Keyboard->Esc));
	tkcmd(sprint("bind .edit <Key-%c> {send edit esc}", Keyboard->Esc));

	tkcmd(sprint("bind .edit <Key-%c> {send edit up}", Keyboard->Up));
	tkcmd(sprint("bind .edit <Key-%c> {send edit down}", Keyboard->Down));

	{
		binds := array[] of {'a', '<', 'b', 'd', 'e','>', 'f', 'h', 'k', 'n', 'o', 'p', 'u', 'v', 'w'};
		for(i := 0; i < len binds; i++)
			tkcmd(sprint("bind .m.text <Control-\\%c> {}", binds[i]));
		binds = array[] of {
			Keyboard->Home, Keyboard->Left, Keyboard->End, Keyboard->Right,
			Keyboard->Del, Keyboard->Down, Keyboard->Up, Keyboard->Pgdown, Keyboard->Pgup
		};
		for(i = 0; i < len binds; i++)
			tkcmd(sprint("bind .m.text <Key-\\%c> {}", binds[i]));

		binds = array[] of {'h', 'w', 'u', 'f', 'b', 'y', 'e', 'l', 'g', 'r'};
		for(i = 0; i < len binds; i++)
			tkcmd(sprint("bind .m.text <Control-\\%c> {send ctl %c}", binds[i], binds[i]));

	}

	if(filename != nil && filefd == nil)
		statuswarn(sprint("new file %q", filename));
	if(filefd != nil)
		textfill(filefd);
	tkcmd(".m.text insert end \u0003");
	tkcmd(".m.text tag add eof {end -1c} end");
	up();

	modeset(Command0);

	tkclient->onscreen(top, nil);
	tkclient->startinput(top, "kbd"::"ptr"::nil);

	for(;;) alt {
	s := <-top.ctxt.kbd =>
		tk->keyboard(top, s);

	s := <-top.ctxt.ptr =>
		tk->pointer(top, *s);

	s := <-top.ctxt.ctl or
	s = <-top.wreq =>
		tkclient->wmctl(top, s);

	menu := <-wmctl =>
		case menu {
		"exit" =>	quit();
		* =>		tkclient->wmctl(top, menu);
		}

	txt := <-textc =>
		# special keys/mouse from text widget
		statustext = nil;
		say(sprint("text: %q", txt));
		case txt {
		"esc" =>
			if(change != nil) {
				# xxx should probably only handle insert mode here?
				changeadd(change);
				change = nil;
			}
			modeset(Command0);
		"b1down" =>
			modeset(Command0);
		"b1up" =>
			cursorset(text.pos(indexget()));
			ranges := tkcmd(".m.text tag ranges sel");
			if(ranges != nil) {
				(nil, l) := sys->tokenize(ranges, " ");
				if(len l != 2) {
					tkcmd(".m.text tag remove sel "+ranges);
					warn(sprint("bad selection range %q?", ranges));
					continue;
				}
				cursor = text.pos(Pos.parse(hd l));
				modeset(Visual);
				cursor = text.pos(Pos.parse(hd tl l));
			}
		* =>
			warn(sprint("text unhandled, %q", txt));
		}
		up();

	key := <-keyc =>
		# keys from text widget
		statustext = nil;
		say(sprint("cmd: %q", key));
		(x, rem) := str->toint(key, 16);
		if(rem != nil) {
			warn(sprint("bogus char code %q, ignoring", key));
			continue;
		}
		case mode {
		Insert =>	insert(x);
		Command0 =>	command(x);
		Visual =>	visual(x);
		Edit =>		warn(sprint("key %q while in edit mode?", key));
		}
		up();

	s := <-ctlc  =>
		# control keys from text widget
		statustext = nil;
		say(sprint("ctlc: %q", s));
		if(len s != 1) {
			warn(sprint("bogus ctlc %q", s));
			continue;
		}
		case s[0] {
		'h' =>		cursorset(textdel(Cmod, cursor, cursor.mvchar(-1)));
		'w' =>		cursorset(textdel(Cmod, cursor, cursor.mvword(0, -1)));
		'u' =>		cursorset(textdel(Cmod, cursor, cursor.mvcol(0)));
		'g' =>		statusset();
		'r' =>		redo();
		'l' =>		warn("xxx redraw");
		* =>		warn(sprint("unhandled ctl %q", s));
		}
		up();

	e := <-editc =>
		# special keys from edit widget
		statustext = nil;
		say(sprint("edit: %q", e));
		case e {
		"return" =>
			s := tkcmd(".edit get");
			tkcmd(".edit delete 0 end");
			say(sprint("edit command: %q", s));
			if(s == nil)
				break;
			ss := s[1:];
			case s[0] {
			':' =>	edit(ss);
			'/' or
			'?' =>	searchreverse = (s[0] == '?');
				if(searchset(ss))
					cursorset(search(0, searchreverse, searchregex, cursor));
			* =>	statuswarn(sprint("invalid edit command %q", s));
			}
			edithistput(s);
			modeset(Command0);
		"esc" =>
			tkcmd(".edit delete 0 end");
			edithistcur = -1;
			edithisttext = nil;
			modeset(Command0);
		"up" or
		"down" =>
			# if up/down was down without esc or text editing afterwards,
			# we use the originally typed text to search, not what's currently in the edit field.
			a := l2a(rev(edithist));
			say(sprint("edithist, edithistcur=%d:", edithistcur));
			for(i := 0; i < len a; i++)
				say(sprint("%3d %s", i, a[i]));
			editnavigate(e == "up");
		* =>
			if(str->prefix("press ", e)) {
				(x, rem) := str->toint(e[len "press ":], 16);
				if(rem != nil) {
					warn(sprint("bad edit press %q", e));
					continue;
				}
				# we get up/down and other specials too, they don't change the text
				if((x & Keyboard->Spec) != Keyboard->Spec)
					edithisttext = nil;
			} else
				warn(sprint("unhandled edit command %q", e));
		}
		up();
	}
}

plumb(s: string)
{
	if(!plumbed)
		return statuswarn("cannot plumb");
	msg := ref Msg("vixen", "", sys->fd2path(sys->open(".", Sys->OREAD)), "text", "", array of byte s);
	msg.send();
}

run(s: string)
{
	err := sh->system(drawcontext, s);
	if(err != nil)
		statuswarn(sprint("exit string: %q", err));
}

changeadd(c: ref Change)
{
	if(changeindex < len changes) {
		changes = changes[:changeindex+1];
	} else {
		n := array[len changes+1] of ref Change;
		n[:] = changes;
		changes = n;
	}
	changes[changeindex++] = c;
}

apply(c: ref Change): int
{
	for(l := c.l; l != nil; l = tl l) {
		pick m := hd l {
		Ins =>
			textinsert(text.pos(m.p), m.s);
		Del =>
			textdel(Cnone, text.pos(m.p), text.cursor(m.o+len m.s));
		}
	}
	cursorset(text.pos(c.end));
	return 1;
}

undo()
{
	say(sprint("undo, changeindex=%d, len changes=%d", changeindex, len changes));
	if(changeindex == 0)
		return;
	if(apply(changes[changeindex-1].invert()))
		--changeindex;
}

redo()
{
	say("redo");
	if(changeindex >= len changes)
		return;
	if(apply(changes[changeindex]))
		++changeindex;
}

editset0(index: int, s: string)
{
	edithistcur = index;
	tkcmd(sprint("focus .edit; .edit delete 0 end; .edit insert 0 '%s", s));
}

editset(s: string)
{
	editset0(-1, s);
}

editnavigate(up: int)
{
	if(edithisttext == nil)
		edithisttext = tkcmd(".edit get");
	a := l2a(rev(edithist));
	if(up) {
		for(i := edithistcur+1; i < len a; ++i)
			if(str->prefix(edithisttext, a[i]))
				return editset0(i, a[i]);
	} else {
		for(i := edithistcur-1; i >= 0; --i)
			if(str->prefix(edithisttext, a[i]))
				return editset0(i, a[i]);
	}
	statuswarn("no match");
}

edithistput(s: string)
{
	if(s != nil) {
		edithist = s::edithist;
		edithistcur = -1;
	}
}

searchset(s: string): int
{
	err: string;
	(searchregex, err) = regex->compile(s, 0);
	if(err != nil) {
		searchregex = nil;
		statuswarn("bad search pattern");
		return 0;
	}
	return 1;
}

searchall(re: Regex->Re): array of (int, int)
{
	l: list of (int, int);
	o := 0;
	for(;;) {
		r := regex->executese(re, text.s, (o, len text.s), 1, 1);
		if(len r == 0 || r[0].t0 < 0)
			break;
		l = r[0]::l;
		o = r[0].t1;
	}
	r := array[len l] of (int, int);
	for(i := len r-1; i >= 0; --i) {
		r[i] = hd l;
		l = tl l;
	}
	return r;
}

search(rev, srev: int, re: Regex->Re, cr: ref Cursor): ref Cursor
{
	if(re == nil) {
		statuswarn("no search pattern set");
		return cr;
	}
	if(srev)
		rev = !rev;
	
	r := searchall(re);
	if(len r == 0 || r[0].t0 < 0) {
		statuswarn("pattern not found");
		return cr;
	}
	i: int;
	if(rev) {
		for(i = len r-1; i >= 0; i--)
			if(r[i].t0 < cr.o)
				break;
		if(i < 0) {
			i = len r-1;
			statuswarn("search wrapped");
		}
	} else {
		for(i = 0; i < len r; i++)
			if(r[i].t0 > cr.o)
				break;
		if(i >= len r) {
			i = 0;
			statuswarn("search wrapped");
		}
	}
	newo := r[i].t0;
	return text.cursor(newo);
}

# read & evaluate an address.
address(c: ref Cmd): (ref Cursor, string)
{
	if(!c.more())
		return (nil, nil);
	case x := c.get() {
	'.' =>	return (cursor, nil);
	'$' =>	return (text.pos(Pos (text.lines(), 0)), nil);
	'0' to '9' =>
		return (text.pos(Pos (int c.getnum(), 0)), nil);
	'+' or
	'-' =>
		mult := 1;
		if(x == '-')
			mult = -1;
		return (cursor.mvline(mult*int c.getnum()), nil);
	'/' or
	'?' =>
		return (nil, "search address not yet implemented"); # xxx
	'\'' =>
		if(!c.more())
			return (nil, "incomplete register address");
		# xxx more registers?
		case x = c.get() {
		'<' =>	return (visualstart, nil);
		'>' =>	return (cursor, nil);
		* =>	return (nil, sprint("unknown register %c", x));
		}
	}
	c.unget();
	return (nil, nil);
}

range(c: ref Cmd): (ref Cursor, ref Cursor, string)
{
	if(!c.more())
		return (nil, nil, nil);
	case c.get() {
	'%' =>	return (text.cursor(0), text.end(), nil);
	'*' =>	return (nil, nil, "range '*' not implemented");
	}
	c.unget();
	(cs, err) := address(c);
	ce: ref Cursor;
	if(err == nil && cs != nil && c.char() == ',') {
		c.get();
		(ce, err) = address(c);
	}
	return (cs, ce, err);
}


patternget(c: ref Cmd, sep: int): (string, string)
{
	s := "";
	while(c.more()) {
		x := c.get();
		if(x == sep)
			return (s, nil);
		if(x == '\\' && c.char() == sep)
			x = c.get();
		s[len s] = x;
	}
	return (nil, "missing ending separator");
}

edit(s: string)
{
	if(s == nil)
		return;

	c := Cmd.new(s);
	(cs, ce, err) := range(c);
	if(err != nil)
		return statuswarn("bad range: "+err);
	if(!c.more())
		return statuswarn("missing command");

	case c.get() {
	'!' =>
		# :!command		run command
		# :addr!command		replace line with output from command (that gets the original line as input)
		# :addr,addr!command	replace lines ...
		cmd := c.rem();
		say(sprint("! on %q", cmd));
		if(cs == nil) {
			say(sprint("xxx just execute %q", c.rem()));
		} else {
			if(ce == nil) {
				cs = cs.mvcol(0);
				ce = ce.mvline(1).mvcol(0);
			}
			txt := text.get(cs, ce);
			say(sprint("input is: %q", txt));
			res: string;
			(res, err) = filter(cmd, txt);
			if(err != nil) {
				statuswarn("error: "+err);
				res = err;
			}
			say("result is: "+res);
			cursorset(textdel(Cnewchange, ce, cs));
			textinsert(cursor, res);
		}
	's' =>
		# if no range, then current line (cursor) only.
		# if 1 address, only that line.
		# otherwise, substitute in that range
		if(cs == nil)
			cs = cursor;
		if(ce == nil) {
			cs = cs.mvcol(0);
			ce = cs.mvlineend(1);
		}
		if(!c.more())
			return statuswarn("missing parameters");
		sep := c.get();
		src, dst: string;
		(src, err) = patternget(c, sep);
		if(err == nil)
			(dst, err) = patternget(c, sep);
		if(err != nil)
			return statuswarn("missing parameters");
		g := c.char() == 'g';
		if(g) c.get();
		if(c.more())
			return statuswarn("trailing characters");
		statuswarn(sprint("xxx substitute from %s to %s, pattern %q by %q, g=%d", cs.text(), ce.text(), src, dst, g));
	'r' =>
		# xxx insert text from file.  ignore range, insert on line after cursor.
		filt := c.char() == '!';
		if(filt) c.get();

		while(c.more() && str->in(c.char(), whitespace))
			c.get();
		if(!c.more())
			return statuswarn("missing argument");

		cmd := c.rem();
		res: string;
		if(filt)
			(res, err) = filter(cmd, "");
		else
			(res, err) = readfile(cmd);
		if(err != nil)
			return statuswarn(err);
		textinsert(cursor.mvline(1).mvcol(0), res);
	'w' or
	'q' =>
		# xxx handle range
		if(cs != nil)
			return statuswarn("write with range nog yet implemented"); # xxx
		c.unget();
		w := c.char() == 'w';
		if(w) c.get();
		q := c.char() == 'q';
		if(q) c.get();
		force := c.char() == '!';
		if(force) c.get();

		ofilename: string;
		while(c.more() && str->in(c.char(), whitespace))
			c.get();
		if(c.more()) {
			ofilename = c.rem();
			if(filename == nil)
				filename = ofilename;
		} else
			ofilename = filename;

		err: string;
		if(w) {
			err = textwrite(force, ofilename);
			if(err != nil)
				statuswarn(err);
			else {
				statuswarn("written");
				modified = 0;
			}
		}
		if(q) {
			if(err == nil && (!modified || force))
				quit();
			if(err == nil)
				statuswarn("unsaved changes, use :q!");
		}
	* =>
		warn(sprint("unhandled edit command: %q", s));
	}
}

Aabort, Amore, Achange, Amove: con iota;

# can only return Aabort, Amore, Amove.  not Achange.
move(c: ref Cmd, mult: int, cr: ref Cursor): int
{
	say("move: "+c.text());
	numstr := c.getnum();
	if(!c.more())
		return Amore;
	num := 1;
	if(numstr != nil)
		num = int numstr;
	num *= mult;
	x := c.get();
	case x {
	'j' =>	*cr = *cr.mvline(+num);
	'k' =>	*cr = *cr.mvline(-num);
	'h' =>	*cr = *cr.mvchar(-num);
	'l' =>	*cr = *cr.mvchar(+num);
	'w' =>	*cr = *cr.mvword(0, +num);
	'W' =>	*cr = *cr.mvword(1, +num);
	'b' =>	*cr = *cr.mvword(0, -num);
	'B' =>	*cr = *cr.mvword(1, -num);
	'e' =>	*cr = *cr.mvwordend(0, +num);
	'E' =>	*cr = *cr.mvwordend(1, +num);
	'G' =>	
		if(numstr == nil)
			num = text.lines();
		*cr = *text.pos(Pos (int numstr, 0));
	'$' =>	*cr = *cr.mvlineend(0);
	'^' =>	*cr = *cr.mvcol(0).skipws();
	'n' or
	'N' =>
		rev := x == 'N';
		while(num--)
			*cr = *search(rev, searchreverse, searchregex, cr);
	'*' or
	'#' =>	
		(a, b) := cursor.word();
		if(a == nil) {
			statuswarn("no word under cursor");
			return Amove;
		}
		rev := x == '#';
		ss := text.get(a, b);
		say(sprint("*/#, search for %q", ss));
		(re, err) := regex->compile(ss, 0);
		if(err != nil) {
			statuswarn("bad search pattern (internal error)");  # should not use regexes, but string search
			return Aabort;
		}
		if(rev)
			*cr = *a;
		while(num--)
			*cr = *search(rev, 0, re, cr);
	'%' =>
		if(numstr != nil) {
			# move to percentage of file, in lines
			perc := int numstr;
			if(perc > 0 && perc <= 100)
				*cr = *text.pos(Pos (perc*text.lines()/100, 0));
		} else {
			# move to matching (){}[].  if other char under cursor, search forward for one.
			say(sprint("%% match, c %c (%d)", cr.char(), cr.char()));
			if(cr.char() < 0)
				break;
			nc := cr.clone();
			if(!str->in(nc.char(), "(){}[]"))
				nc = nc.findchar("(){}[]", 0);
			if(nc != nil)
				case nc.char() {
				'(' =>	nc = nc.findchar(")", 0);
				')' =>	nc = nc.findchar("(", 1);
				'[' =>	nc = nc.findchar("]", 0);
				']' =>	nc = nc.findchar("[", 1);
				'{' =>	nc = nc.findchar("}", 0);
				'}' =>	nc = nc.findchar("{", 1);
				}
			if(nc != nil)
				*cr = *nc;
		}
	'|' =>
		n := 0;
		if(numstr != nil)
			n = int numstr;
		*cr = *cr.mvcol(n*mult);
	'(' =>
		# beginning of previous sentence
		Lineend: con ".!?:";
		nc := cr.clone();
		xx := nc.prev();
		while(xx >= 0 && (str->in(xx, Lineend) || str->in(xx, whitespace)))
			xx = nc.prev();
		nc = nc.findchar(Lineend, 1);
		if(nc != nil) {
			nc = nc.skip(Lineend);
			nc = nc.skipws();
			*cr = *nc;
		} else
			*cr = *text.cursor(0);
	')' =>
		Lineend: con ".!?:";
		nc := cr.clone();
		nc = nc.skip("^"+Lineend);
		nc = nc.skip(Lineend+whitespace);
		*cr = *nc;
	'{' => 
		nc := cr.clone();
		xx := nc.prev();
		while(xx == '\n')
			xx = nc.prev();
		nc = nc.findstr("\n\n", 1);
		if(nc == nil)
			nc = text.cursor(0);
		else
			nc.next();
		*cr = *nc;
	'}' =>
		nc := cr.clone();
		xx := nc.char();
		while(xx == '\n')
			xx = nc.next();
		nc = nc.findstr("\n\n", 0);
		if(nc == nil)
			nc = text.end();
		else
			nc.next();
		*cr = *nc;
	'`' =>
		if(!c.more())
			return Amore;
		xx := c.get();
		if(xx >= len marks) {
			statuswarn("invalid mark register");
			return Aabort;
		}
		pos := marks[xx];
		if(pos == nil)
			return Amove;
		*cr = *text.pos(*pos);
	# H M L?
	* =>
		warn(sprint("unhandled move %c", x));
		return Aabort;
	}
	return Amove;
}

insert(x: int)
{
	textinsert(nil, sprint("%c", x));
}

visual(x: int)
{
	commandcur[len commandcur] = x;
	c := Cmd.new(commandcur);
	case visual0(c) {
	Aabort or
	Achange =>
		commandcur = nil;
		modeset(Command0);
	Amove =>
		commandcur = nil;
	Amore =>
		;
	}
}

visual0(c: ref Cmd): int
{
	c.getnum1();
	if(!c.more())
		return Amore;
	case x := c.get() {
	'd' =>
		cursorset(textdel(Cnewchange, cursor, visualstart));
	'y' =>
		registerput(register, text.get(visualstart, cursor));
	'J' =>
		(cs, ce) := Cursor.order(visualstart, cursor);
		join(cs, ce);
	'<' or
	'>' =>
		(cs, ce) := Cursor.order(visualstart, cursor);
		indent(cs.mvcol(0), ce.mvlineend(0), x == '<');
	'z' =>
		plumb(text.get(visualstart, cursor));
	'Z' =>
		run(text.get(visualstart, cursor));
	'o' =>
		tmp := cursor;
		cursor = visualstart;
		visualstart = tmp;
		return Amove;
	'!' =>
		modeset(Edit);
		editset(":'<,'>!");
		commandcur = nil;
		return Amore; # xxx abuse of Amore.
	':' =>
		modeset(Edit);
		editset(":'<,'>");
		commandcur = nil;
		return Amore; # xxx abuse of Amore.
	'"' =>
		if(!c.more())
			return Amore;
		xx := c.get();
		if(xx >= len registers)
			return Aabort;
		register = xx;
		return visual0(c);  # xxx evil/ugly
	* =>
		cmd := Cmd.new(commandcur);
		case move(cmd, 1, nc := cursor.clone()) {
		Aabort =>
			warn(sprint("unhandled visual command"));
			commandcur = nil;
			return Amore;
		Amore =>
			commandcur = nil;
			return Amore;
		Amove =>
			visualset(nc);
			cursorset(nc);
			return Amove;
		}
	}
	return Achange;
}

visualset(c: ref Cursor)
{
	a := visualstart;
	b := c;
	if(a.o > b.o) {
		tmp := a;
		a = b;
		b = tmp;
	}
	say(sprint("visualset, from %s to %s", a.pos.text(), b.pos.text()));
	tkcmd(".m.text tag remove sel 1.0 end");
	tkcmd(sprint(".m.text tag add sel %s %s", a.pos.text(), b.pos.text()));
}


command(x: int)
{
	case x {
	Keyboard->Esc =>
		commandcur = nil;
	* =>
		commandcur[len commandcur] = x;
		a := command0(commandcur);
		case a {
		Aabort =>
			commandcur = nil;
		Amore =>
			;
		Achange =>
			commandprev = commandcur;
			commandcur = nil;
		Amove =>
			commandcur = nil;
		}
		if(a != Amore)
			register = '"';
	}
}


commandmove(c: ref Cmd, num1, end: int): (int, int, ref Cursor)
{
	if(!c.more())
		return (Amore, 0, nil);
	cc := c.clone();
	c.getnum2();
	num2 := c.num2(1);
	x := c.char();
	if(x == -1)
		return (Amore, 0, nil);
	if(x == end)
		return (Achange, num2, nil);
	a := move(cc, num1, nc := cursor.clone());
	say(sprint("%c, move %d, cursor %s, nc %s", end, a, cursor.text(), nc.text()));
	return (a, num2, nc);
}

command0(s: string): int
{
	c := Cmd.new(s);
	say("command0: "+c.text());
	if(c.isnum())
		c.getnum1();
	if(!c.more())
		return Amore;  # just a count so far
	num1 := c.num1(1);

	case x := c.get() {
	'/' or
	'?' =>
		modeset(Edit);
		editset(sprint("%c", x));
		commandcur = nil;
	'v' =>
		modeset(Visual);
		visualmode = Char;
		commandcur = nil;
	'V' =>
		modeset(Visual);
		visualmode = Line;
		commandcur = nil;
	':' =>
		modeset(Edit);
		editset(":");
		commandcur = nil;

	'i' =>
		modeset(Insert);
	'I' =>
		cursorset(cursor.mvcol(0).skipws());
		modeset(Insert);
	'a' =>
		cursorset(cursor.mvchar(+1));
		modeset(Insert);
	'A' =>
		cursorset(cursor.mvlineend(0));
		modeset(Insert);
	'o' =>
		cursorset(cursor.mvlineend(0));
		textinsert(nil, "\n");
		modeset(Insert);
	'O' =>
		cursorset(cursor.mvcol(0));
		textinsert(nil, "\n");
		cursorset(cursor.mvline(-1));
		modeset(Insert);

	'x' =>
		cursorset(textdel(Cnewchange, cursor.mvchar(num1), cursor));
	'd' =>
		(a, num2, nc) := commandmove(c, num1, 'd');
		case a {
		Amore or
		Aabort =>
			return a;
		Amove =>
			cursorset(textdel(Cnewchange, nc, cursor));
		Achange =>
			origpos := cursor.pos;
			textdel(Cnewchange, cursor.mvcol(0), cursor.mvline(num1*num2).mvcol(0));
			cursorset(text.pos(origpos).mvcol(0).skipws());
		}
	'D' =>
		cursorset(textdel(Cnewchange, cursor.mvline(num1).mvcol(0), cursor));
	'y' =>
		(a, num2, nc) := commandmove(c, num1, 'y');
		txt: string;
		case a {
		Amore or
		Aabort =>	return a;
		Amove =>	txt = text.get(cursor, nc);
		Achange =>	txt = text.get(cursor.mvcol(0), cursor.mvline(num1*num2).mvcol(0));
		}
		registerput(register, txt);
	'Y' =>
		txt := text.get(cursor.mvcol(0), cursor.mvline(num1).mvcol(0));
		registerput(register, txt);
	'p' =>
		txt := registerget(register);
		if(txt == nil) {
			statuswarn(sprint("nothing in register '%c'", register));
			return Aabort;
		}
		textinsert(nil, txt);
	'P' =>
		txt := registerget(register);
		if(txt == nil) {
			statuswarn(sprint("nothing in register '%c'", register));
			return Aabort;
		}
		cursorset(cursor.mvline(-1));
		textinsert(nil, txt);

	'0' =>
		cursorset(cursor.mvcol(0));

	'u' =>	undo();
	'.' =>
		command0(commandprev);
		return Aabort; # pretend this was aborted so we won't redo it
	'<' or
	'>' =>
		(a, num2, nc) := commandmove(c, num1, x);
		cs, ce: ref Cursor;
		case a {
		Amore or
		Aabort =>	return a;
		Amove =>
			(cs, ce) = Cursor.order(cursor, nc);
		Achange =>
			cs = cursor;
			ce = cs.mvline(max(1, num1*num2-1));
		}
		indent(cs.mvcol(0), ce.mvlineend(0), x == '<');
	'J' =>
		cs := cursor.mvlineend(0);
		ce := cursor.mvline(max(1, num1-1)).mvcol(0);
		join(cs, ce);
	'm' =>
		if(!c.more())
			return Amore;
		xx := c.get();
		if(xx >= len marks) {
			statuswarn("invalid mark register");
			return Aabort;
		}
		marks[xx] = ref cursor.pos;
	'"' =>
		if(!c.more())
			return Amore;
		# xxx this should on be effectuated when really executing this command
		xx := c.get();
		if(xx >= len registers) {
			statuswarn("invalid register");
			return Aabort;
		}
		register = xx;
		return command0(c.rem()); # xxx evil/ugly
	'@' =>
		if(!c.more())
			return Amore;
		xx := c.get();
		if(xx >= len registers) {
			statuswarn("invalid register");
			return Aabort;
		}
		ss := registers[xx];
		if(ss == nil) {
			statuswarn("register empty");
			return Aabort;
		}
		return command0(ss);  # xxx evil/ugly
	# 'r' => replace char
	# 'R' => replace until esc
	* =>
		c = Cmd.new(s);
		case move(c, 1, nc := cursor.clone()) {
		Aabort =>
			warn(sprint("cmd unhandled (move abort)"));
			return Aabort;
		Amore =>
			return Amore;
		Amove =>
			marks['`'] = ref cursor.pos;  # xxx not all movements should count.  not the simple movements...
			cursorset(nc);
			return Amove;
		}
	}
	return Achange;
}

# after delete, cursor will be at b's position
textdel(how: int, a, b: ref Cursor): ref Cursor
{
	n := b;
	(a, b) = Cursor.order(a, b);
	s := text.get(a, b);
	registers[register] = s;
	# xxx restrict a and b to range
	case how {
	Cnone =>
		;
	Cmod =>
		warn("xxx textdel with Cmod");
	Cnewchange =>
		changeadd(ref Change (a.pos, b.pos, list of {ref Mod.Del (a.o, a.pos, s)}));
	Cnewmod =>
		warn("xxx textdel with Cnewmod...");
	* =>
		raise "missing case in textdel";
	}
	tkcmd(sprint(".m.text delete %s %s", a.pos.text(), b.pos.text()));
	text.del(a, b);
	marks['"'] = ref n.pos;
	return n;
}

textinsert(c: ref Cursor, s: string)
{
	if(c == nil)
		c = cursor;
	tkcmd(sprint(".m.text insert %s '%s", c.pos.text(), s));
	tkcmd(sprint(".m.text tag remove eof %s {%s +%dc}", c.pos.text(), c.pos.text(), len s));
	cursor = text.ins(c, s);
	marks['"'] = ref cursor.pos;
	tkcursorset(cursor);
	modified = 1;
	say(sprint("textinsert, inserted %q, cursor now %s", s, cursor.text()));
}

dropindent(c: ref Cmd)
{
	for(i := 8; i > 0; i--)
		case c.get() {
		' ' =>	;
		'\t' =>	return;
		* =>	c.unget(); return;
		}
}

# (un)indent text
indent(cs, ce: ref Cursor, rev: int)
{
	s := text.get(cs, ce);
	r: string;
	if(rev) {
		c := Cmd.new(s);
		while(c.more()) {
			dropindent(c);
			while((x := c.get()) >= 0) {
				r[len r] = x;
				if(x == '\n')
					break;
			}
		}
	} else {
		r[len r] = '\t';
		for(i := 0; i < len s; i++) {
			r[len r] = s[i];
			if(s[i] == '\n' && i+1 < len s)
				r[len r] = '\t';
		}
	}
	
	cursor = textdel(Cnewchange, ce, cs);
	textinsert(cursor, r);
	cursorset(cs.mvcol(0).skipws());
}

# remove empty lines, replace newline by a space
join(cs, ce: ref Cursor)
{
	if(cs.pos.l == ce.pos.l)
		ce = ce.mvline(1);
	cs = cs.mvlineend(0);
	ce = ce.mvcol(0);
	s := text.get(cs, ce);

	r := "";
	for(i := 0; i < len s; i++)
		case s[i] {
		'\n' =>
			r[len r] = ' ';
			while(i < len s && s[i] == '\n')
				++i;
		* =>
			r[len r] = s[i];
		}

	cursor = textdel(Cnewchange, ce, cs);
	textinsert(cursor, r);
}

cursorset(c: ref Cursor)
{
	say(sprint("new cursor: %s", c.text()));
	cursor = c;
	tkcursorset(c);
}

tkcursorset(c: ref Cursor)
{
	tkcmd(sprint(".m.text mark set insert %s", c.pos.text()));
	tkcmd(sprint(".m.text see %s", c.pos.text()));
}

up()
{
	if(dflag)
		statusset();  # xxx for now
	tkcmd("update");
}

indexget(): Pos
{
	s := tkcmd(".m.text index insert");
	(l, c) := str->splitstrl(s, ".");
	if(c == nil)
		raise sprint("bad insert index: %q", s);
	return Pos (int l, int c[1:]);
}

indexset(p: Pos)
{
	tkcmd(sprint(".m.text mark set insert %d.%d", p.l, p.c));
}

textfill(fd: ref Sys->FD)
{
	buf := array[Sys->ATOMICIO] of byte;
	for(;;) {
		n := sys->read(fd, buf, len buf);
		if(n == 0)
			break;
		if(n < 0)
			fail(sprint("read: %r"));
		tkcmd(".m.text insert end '"+string buf[:n]); # xxx should only convert whole utf-8 chars
	}
}

textwrite(force: int, f: string): string
{
	fd: ref Sys->FD;
	if(f == nil)
		return "no filename set, use ':w filename'";
	if(filefd == nil || f != filename) {
		fd = sys->create(f, Sys->ORDWR, 8r666);
		if(fd == nil)
			return sprint("create %q: %r", f);
		if(f == filename)
			filefd = fd;
	} else {
		sys->seek(filefd, big 0, Sys->SEEKSTART);
		fd = filefd;
	}
	return bufwritefd(text, fd);
}

statuswarn(s: string)
{
	say("statuswarn: "+s);
	statustext = s;
	statusset();
}

statusset()
{
	s := sprint("%9s ", "("+modes[mode]+")");
	if(filename == nil)
		s += "(no filename)";
	else
		s += sprint("%q", filename);
	s += sprint(", %4d lines, %5d chars, pos %s, %s", text.lines(), text.chars(), cursor.pos.text(), commandcur);
	if(statustext != nil)
		s += ", "+statustext;
	tkcmd(sprint(".s.status configure -text '%s", s));
}

modeset(m: int)
{
	case m {
	Visual =>	visualstart = cursor;
	}
	if(m == Command0)
		tkcmd(".m.text tag remove sel 1.0 end");
	case m {
	Insert or
	Command0 or
	Visual =>	tkcmd("focus .m.text");
	}
	if(m == Command0)
		commandcur = nil;
	mode = m;
	statusset();
}

registerget(c: int): string
{
	if(c >= len registers)
		return nil;
	if(c == '*')
		return tkclient->snarfget();
	return registers[register];
}

registerput(c: int, s: string): string
{
	if(c >= len registers)
		return "bad register";
	if(c == '*')
		tkclient->snarfput(s);
	else
		registers[c] = s;
	return nil;
}

readfile(f: string): (string, string)
{
	b := bufio->open(f, Bufio->OREAD);
	if(b == nil)
		return (nil, sprint("open: %r"));
	s := "";
	for(;;)
	case c := b.getc() {
	Bufio->EOF =>	return (s, nil);
	Bufio->ERROR =>	return (nil, sprint("read: %r"));
	* =>		s[len s] = c;
	}
}

hasnewline(s: string): int
{
	for(i := 0; i < len s; i++)
		if(s[i] == '\n')
			return 1;
	return 0;
}

tkcmd(s: string): string
{
	r := tk->cmd(top, s);
	if(r != nil && r[0] == '!')
		warn(sprint("tkcmd: %q: %s", s, r));
	return r;
}

tkcmds(a: array of string)
{
	for(i := 0; i < len a; i++)
		tkcmd(a[i]);
}

quit()
{
	killgrp(pid());
	exit;
}

say(s: string)
{
	if(dflag)
		warn(s);
}
