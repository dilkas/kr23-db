problems([p01]).


testDomain(p01, [objects(floor, [top,f1,bottom]), objects(maxobj, [a,a1])]).

%initState(p01, [
%at(f1),
%pup(bottom),
%pdown(f2),
%up,
%above(top,f1),
%above(top,f2),
%above(top,bottom),
%above(f2,f1),
%above(f2,bottom),
%above(f1,bottom)
%]).


initState(p01, [
at(f1),
waiting(bottom,top)
]).

horizon(p01,7).


