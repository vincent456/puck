:-ensure_loaded('/home/lorilan/thesis/projects/constraintSolver/pl_eval/evaluator.pl').
node('root.calculator.enter',object, 'enter').
node('dynamic4.add',object, 'dynamic4.add').
node('dynamic3.add',object, 'dynamic3.add').
node('dynamic2.equals',object, 'dynamic2.equals').
node('root.calculator.arg',object, 'arg').
node('root.calculator.equals.s',object, 's').
node('root.calculator.equals',object, 'equals').
node('root.calculator.acc',object, 'acc').
node('root.calculator.equals',object, 'equals').
node('dynamic1.equals',object, 'dynamic1.equals').
node('root.calculator.arg',object, 'arg').
node('root.calculator.acc',object, 'acc').
node('root.calculator.add.s2',object, 's2').
node('root.calculator.add.s',object, 's').
node('root.calculator.add',object, 'add').
node('root.calculator.arg',object, 'arg').
node('root.calculator.enter.n',object, 'n').
node('root.calculator.enter.s',object, 's').
node('root.calculator.enter',object, 'enter').
node('root.calculator.acc',object, 'acc').
node('root.calculator.arg',object, 'arg').
node('root.calculator',object, 'calculator').
node('root',object, 'root').
edge(uses, 'root', 'root.calculator.enter').
edge(uses, 'root', 'dynamic4.add').
edge(uses, 'root', 'dynamic3.add').
edge(uses, 'root', 'dynamic2.equals').
edge(uses, 'root.calculator.equals', 'root.calculator.arg').
edge(contains, 'root.calculator.equals', 'root.calculator.equals.s').
edge(contains, 'root.calculator', 'root.calculator.equals').
edge(uses, 'root.calculator.add', 'root.calculator.acc').
edge(uses, 'root.calculator.add', 'root.calculator.equals').
edge(uses, 'root.calculator.add', 'dynamic1.equals').
edge(uses, 'root.calculator.add', 'root.calculator.arg').
edge(uses, 'root.calculator.add', 'root.calculator.acc').
edge(contains, 'root.calculator.add', 'root.calculator.add.s2').
edge(contains, 'root.calculator.add', 'root.calculator.add.s').
edge(contains, 'root.calculator', 'root.calculator.add').
edge(uses, 'root.calculator.enter', 'root.calculator.arg').
edge(contains, 'root.calculator.enter', 'root.calculator.enter.n').
edge(contains, 'root.calculator.enter', 'root.calculator.enter.s').
edge(contains, 'root.calculator', 'root.calculator.enter').
edge(contains, 'root.calculator', 'root.calculator.acc').
edge(contains, 'root.calculator', 'root.calculator.arg').
edge(contains, 'root', 'root.calculator').
:-pl2dot.
:-halt.
