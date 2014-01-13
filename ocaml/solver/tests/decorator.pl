node('decorator', package, 'decorator').
node('decorator.candidate', package, 'candidate').
edge(contains,'decorator','decorator.candidate').
node('decorator.candidate.AwithY', class, 'AwithY').
edge(contains,'decorator.candidate','decorator.candidate.AwithY').
node('decorator.candidate.AwithY.AwithY#_void', constructor, 'AwithY','#_void').
edge(contains,'decorator.candidate.AwithY','decorator.candidate.AwithY.AwithY#_void').
node('decorator.candidate.A.A#_void', constructor, 'A','#_void').
edge(uses,'decorator.candidate.AwithY.AwithY#_void','decorator.candidate.A.A#_void').
node('decorator.candidate.AwithY.doIt__void', method, 'doIt','__void').
edge(contains,'decorator.candidate.AwithY','decorator.candidate.AwithY.doIt__void').
node('decorator.candidate.A.doIt__void', method, 'doIt','__void').
edge(uses,'decorator.candidate.AwithY.doIt__void','decorator.candidate.A.doIt__void').
node('decorator.candidate.AwithY.doY__void', method, 'doY','__void').
edge(uses,'decorator.candidate.AwithY.doIt__void','decorator.candidate.AwithY.doY__void').
edge(contains,'decorator.candidate.AwithY','decorator.candidate.AwithY.doY__void').
node('decorator.candidate.A', class, 'A').
edge(isa,'decorator.candidate.AwithY','decorator.candidate.A').
%edge(uses,'decorator.candidate.AwithY','decorator.candidate.A').
edge(contains,'decorator.candidate','decorator.candidate.A').
edge(contains,'decorator.candidate.A','decorator.candidate.A.A#_void').
edge(contains,'decorator.candidate.A','decorator.candidate.A.doIt__void').
node('decorator.candidate.AwithZ', class, 'AwithZ').
edge(contains,'decorator.candidate','decorator.candidate.AwithZ').
node('decorator.candidate.AwithZ.doIt__void', method, 'doIt','__void').
edge(contains,'decorator.candidate.AwithZ','decorator.candidate.AwithZ.doIt__void').
node('decorator.candidate.AwithZ.doZ__void', method, 'doZ','__void').
edge(uses,'decorator.candidate.AwithZ.doIt__void','decorator.candidate.AwithZ.doZ__void').
edge(uses,'decorator.candidate.AwithZ.doIt__void','decorator.candidate.A.doIt__void').
edge(contains,'decorator.candidate.AwithZ','decorator.candidate.AwithZ.doZ__void').
node('decorator.candidate.AwithZ.AwithZ#_void', constructor, 'AwithZ','#_void').
edge(contains,'decorator.candidate.AwithZ','decorator.candidate.AwithZ.AwithZ#_void').
edge(uses,'decorator.candidate.AwithZ.AwithZ#_void','decorator.candidate.A.A#_void').
edge(isa,'decorator.candidate.AwithZ','decorator.candidate.A').
%edge(uses,'decorator.candidate.AwithZ','decorator.candidate.A').
node('decorator.candidate.AwithXY', class, 'AwithXY').
edge(contains,'decorator.candidate','decorator.candidate.AwithXY').
node('decorator.candidate.AwithXY.AwithXY#_void', constructor, 'AwithXY','#_void').
edge(contains,'decorator.candidate.AwithXY','decorator.candidate.AwithXY.AwithXY#_void').
node('decorator.candidate.AwithX.AwithX#_void', constructor, 'AwithX','#_void').
edge(uses,'decorator.candidate.AwithXY.AwithXY#_void','decorator.candidate.AwithX.AwithX#_void').
node('decorator.candidate.AwithXY.doIt__void', method, 'doIt','__void').
edge(contains,'decorator.candidate.AwithXY','decorator.candidate.AwithXY.doIt__void').
node('decorator.candidate.AwithX.doIt__void', method, 'doIt','__void').
edge(uses,'decorator.candidate.AwithXY.doIt__void','decorator.candidate.AwithX.doIt__void').
node('decorator.candidate.AwithXY.obj', attribute, 'obj').
edge(uses,'decorator.candidate.AwithXY.doIt__void','decorator.candidate.AwithXY.obj').
edge(uses,'decorator.candidate.AwithXY.doIt__void','decorator.candidate.AwithY.doY__void').
edge(contains,'decorator.candidate.AwithXY','decorator.candidate.AwithXY.obj').
edge(uses,'decorator.candidate.AwithXY.obj','decorator.candidate.AwithY.AwithY#_void').
edge(uses,'decorator.candidate.AwithXY.obj','decorator.candidate.AwithY').
node('decorator.candidate.AwithX', class, 'AwithX').
edge(isa,'decorator.candidate.AwithXY','decorator.candidate.AwithX').
%edge(uses,'decorator.candidate.AwithXY','decorator.candidate.AwithX').
node('decorator.candidate.DecoratorDemo', class, 'DecoratorDemo').
edge(contains,'decorator.candidate','decorator.candidate.DecoratorDemo').
node('decorator.candidate.DecoratorDemo.main__java.lang.String', method, 'main','__java.lang.String').
edge(contains,'decorator.candidate.DecoratorDemo','decorator.candidate.DecoratorDemo.main__java.lang.String').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.A').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithXY.AwithXY#_void').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithX.AwithX#_void').
node('decorator.candidate.AwithXYZ.AwithXYZ#_void', constructor, 'AwithXYZ','#_void').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithXYZ.AwithXYZ#_void').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithXY').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.A.doIt__void').
node('decorator.candidate.AwithXYZ', class, 'AwithXYZ').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithXYZ').
edge(uses,'decorator.candidate.DecoratorDemo.main__java.lang.String','decorator.candidate.AwithX').
node('decorator.candidate.DecoratorDemo.DecoratorDemo#_void', constructor, 'DecoratorDemo','#_void').
edge(contains,'decorator.candidate.DecoratorDemo','decorator.candidate.DecoratorDemo.DecoratorDemo#_void').
edge(contains,'decorator.candidate','decorator.candidate.AwithXYZ').
edge(contains,'decorator.candidate.AwithXYZ','decorator.candidate.AwithXYZ.AwithXYZ#_void').
edge(uses,'decorator.candidate.AwithXYZ.AwithXYZ#_void','decorator.candidate.AwithX.AwithX#_void').
node('decorator.candidate.AwithXYZ.obj1', attribute, 'obj1').
edge(contains,'decorator.candidate.AwithXYZ','decorator.candidate.AwithXYZ.obj1').
edge(uses,'decorator.candidate.AwithXYZ.obj1','decorator.candidate.AwithY.AwithY#_void').
edge(uses,'decorator.candidate.AwithXYZ.obj1','decorator.candidate.AwithY').
node('decorator.candidate.AwithXYZ.obj2', attribute, 'obj2').
edge(contains,'decorator.candidate.AwithXYZ','decorator.candidate.AwithXYZ.obj2').
edge(uses,'decorator.candidate.AwithXYZ.obj2','decorator.candidate.AwithZ').
edge(uses,'decorator.candidate.AwithXYZ.obj2','decorator.candidate.AwithZ.AwithZ#_void').
node('decorator.candidate.AwithXYZ.doIt__void', method, 'doIt','__void').
edge(contains,'decorator.candidate.AwithXYZ','decorator.candidate.AwithXYZ.doIt__void').
edge(uses,'decorator.candidate.AwithXYZ.doIt__void','decorator.candidate.AwithX.doIt__void').
edge(uses,'decorator.candidate.AwithXYZ.doIt__void','decorator.candidate.AwithXYZ.obj1').
edge(uses,'decorator.candidate.AwithXYZ.doIt__void','decorator.candidate.AwithXYZ.obj2').
edge(uses,'decorator.candidate.AwithXYZ.doIt__void','decorator.candidate.AwithZ.doZ__void').
edge(uses,'decorator.candidate.AwithXYZ.doIt__void','decorator.candidate.AwithY.doY__void').
edge(isa,'decorator.candidate.AwithXYZ','decorator.candidate.AwithX').
%edge(uses,'decorator.candidate.AwithXYZ','decorator.candidate.AwithX').
edge(contains,'decorator.candidate','decorator.candidate.AwithX').
edge(contains,'decorator.candidate.AwithX','decorator.candidate.AwithX.doIt__void').
edge(uses,'decorator.candidate.AwithX.doIt__void','decorator.candidate.A.doIt__void').
node('decorator.candidate.AwithX.doX__void', method, 'doX','__void').
edge(uses,'decorator.candidate.AwithX.doIt__void','decorator.candidate.AwithX.doX__void').
edge(contains,'decorator.candidate.AwithX','decorator.candidate.AwithX.AwithX#_void').
edge(uses,'decorator.candidate.AwithX.AwithX#_void','decorator.candidate.A.A#_void').
edge(contains,'decorator.candidate.AwithX','decorator.candidate.AwithX.doX__void').
edge(isa,'decorator.candidate.AwithX','decorator.candidate.A').
%edge(uses,'decorator.candidate.AwithX','decorator.candidate.A').


hideFrom('decorator.candidate.A', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithZ'). 
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithX', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithY', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithXYZ').
 
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithZ','decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithXY'). 


