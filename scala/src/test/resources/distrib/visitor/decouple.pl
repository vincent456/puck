java_import(['visitor.candidate', 
	'visitor.candidate.BinaryTreeLeaf',
	'visitor.candidate.BinaryTreeNode',
	'visitor.candidate.BinaryTree']).

declareSet(outside_operations, ['sum__void', 'traverse__void']).

declareSet(structure, ['BinaryTreeNode', 'BinaryTreeLeaf', 'BinaryTree']).

hideSetFrom(outside_operations, structure).