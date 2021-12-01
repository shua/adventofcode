input string 1113122113
> + > + > + > +++ > + > ++ > ++ > + > + > +++ > 
end of array contains counter 40
+++++ +++++  +++++ +++++  +++++ +++++  +++++ +++++  +++++ +++++

+++++ +++++  +++++ +++++  +++++ +++++  ++ . PRINTABLE
----- -----  ----- -----  ----- -----  --

:__arr__ c
:        |
[
	: x __1st__ c 0 1 y
	>> +       1
	<< [<] >   x
	[ [>] >> + <<< [<] > - ]   y:=x

	: |
	: 0 __1st__ l c 0
	> [>] <<    l
	[
		:copy x to y
		: 0 x __1st__ 0 __2nd__ 0 y
		[<] >   x
		[ 
			[>]     end 1st
			> [>]   lst 2nd
			> + <<  y
			[<]     beg 2nd
			< [<] > x
			- 
		]
		: 0 1 __crumbs__ 1 0 __1st__ 0
		: leaving breadcrumbs to top of stack
		<+>
		last of 2nd array
		> [>] > [>]

		:    |
		:n x 0 y t s
		:t = x my
		<  [->+>>+<<<]  x to 0;t
		>  [-<+>]       0 to x
		>  [->->+<<]    t my; y to s
		>> [-<<+>>]     s to y

		:t is now 0 if x=y
		<[  if t\=0
			:most negative is (1 m 3 = n2)
			++ 
			:(0 n1 n2 1 2) now ge 0
			[-]
			:3 after last
			>>
		]
		:n x 0 y 0 0 0
		:if t        |
		:else    |


		:        |
		:n x 0 k 0
		<[-]   k:=0
		<< < + increment n
		:lst 1st
		[<] <<
	]

	:0 __crumbs__ 0 c 0
	:             |
	+> crumb; c

	:c 0 __2nd__ 0
	>> [>] + [<] <
	:c 0 __2nd__ d 0
	[ - >> [>] < + [<] < ]   d:=c
	+>> [>] < -  crumb; decrement d

	:move array over crumbs
	: 0 h i __crumbs__ 0 __arr__ d 0
	:                            |
	[<]<[<]> h
	-       
	>[>]

	: 0 0 1 __crumbs__ 0 h __arr__ 0
	:                  |
	>[ h
		:__to__ 0 1 __crumbs__ 0 h __from__ 0
		<<[<]>->[>]>
		:__to__ 0 0 __crumbs__ 0 h __from__ 0
		[ - <<[<]< + >>[>]> ]
		:__to__ h 0 __crumbs__ 0 0 __from__ 0
		<+
		:__to__ h 0 __crumbs__ 1 0 __from__ 0
		>>   next
	]

	: 0 __arr__ 0 __crumbs__ 0 0
	:                          |
	<<[-<] remove __crumbs__

	: 0 __arr__ d 0
	:             |
	<- d m1

  +++++ +++++  +++++ +++++  +++++ +++++  ++ . PRINTABLE
  ----- -----  ----- -----  ----- -----  --
]
+++++ +++++ . NL 
----- -----

:printlen
:  before I would first store the length
:  and then print the number; but the arrays got longer than 255
:  more verbose version can be found in printlen_bf

:0 __arr__ 0 h __num__ 0 p c
:          |
>+< h:='0'
<[  lst __arr__
	[<]>[-]>[>]>[>] pop __arr__
	>>+<< c:=1

	<[  lst __num__
		[<]> [ [>]>> + <<<[<]> - ]  add cur __num__ to c
		>[>]> [ <<[<]< + >>[>]> - ] 
		> [-<+>]
	
		+<----- ----- -<+>
		[ +++++ +++++ >-
			<[-<+>]
		]
		<[->+<]
		<
	]
	>>[-<<+>>]
	>[<<++>>->]

	<<<[<]<
]

>>[>]< [
+++++ +++++  +++++ +++++  +++++ +++++  +++++ +++++  +++++ ++ . NUM
----- -----  ----- -----  ----- -----  ----- -----  ----- -- <
]
+++++ +++++ . NL
----- -----

