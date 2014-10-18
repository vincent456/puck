%java_import(['visitor.candidate', 
%	'visitor.candidate.BinaryTreeLeaf',
%	'visitor.candidate.BinaryTreeNode',
%	'visitor.candidate.BinaryTree']).

%declareSet(outside_operations, ['sum__void', 'traverse__void']).

java_import(['visitor.candidate']).

declareSet(outside_operations, ['BinaryTreeNode.sum__void', 'BinaryTreeNode.traverse__void',
	'BinaryTreeLeaf.sum__void', 'BinaryTreeLeaf.traverse__void',
	'BinaryTree.sum__void', 'BinaryTree.traverse__void']).

declareSet(structure, ['BinaryTreeNode', 'BinaryTreeLeaf', 'BinaryTree']).

hideSetFrom(outside_operations, structure).