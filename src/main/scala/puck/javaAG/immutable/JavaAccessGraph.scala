package puck.javaAG.immutable

import puck.graph.immutable.AccessGraph._
import puck.graph.immutable.transformations.Recording
import puck.graph.immutable.AccessGraph
import puck.javaAG.immutable.nodeKind.JavaNodeKind
import puck.util.{PuckNoopLogger, PuckLogger}

/**
 * Created by lorilan on 29/10/14.
 */
class JavaAccessGraph
(program : AST.Program,
 logger : PuckLogger = PuckNoopLogger,
 idSeed : () => Int,
 nodesSet : NodeSet[JavaNodeKind],
 usersMap : EdgeMap[JavaNodeKind],
 usesMap  : EdgeMap[JavaNodeKind],
 contentsMap  : EdgeMap[JavaNodeKind],
 containerMap : Node2NodeMap[JavaNodeKind],
 superTypesMap : EdgeMap[JavaNodeKind],
 subTypesMap : EdgeMap[JavaNodeKind],
 dominantUsesMap : UseDependencyMap[JavaNodeKind],
 dominatedUsesMap : UseDependencyMap[JavaNodeKind],
 abstractionsMap : AbstractionMap[JavaNodeKind],
 recording : Recording[JavaNodeKind])
  extends AccessGraph[JavaNodeKind](JavaNode, logger, idSeed, nodesSet,
  usersMap, usesMap, contentsMap, containerMap, superTypesMap, subTypesMap,
  dominantUsesMap, dominatedUsesMap, abstractionsMap, recording){

  override def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeSet[JavaNodeKind] = nodesSet,
               nUsersMap : EdgeMap[JavaNodeKind] = usersMap,
               nUsesMap  : EdgeMap[JavaNodeKind] = usesMap,
               nContentMap  : EdgeMap[JavaNodeKind] = contentsMap,
               nContainerMap : Node2NodeMap[JavaNodeKind] = containerMap,
               nSuperTypesMap : EdgeMap[JavaNodeKind] = superTypesMap,
               nSubTypesMap : EdgeMap[JavaNodeKind] = subTypesMap,
               nDominantUsesMap : UseDependencyMap[JavaNodeKind] = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap[JavaNodeKind] = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap[JavaNodeKind] = abstractionsMap,
               nRecording : Recording[JavaNodeKind] = recording) : AccessGraph[JavaNodeKind] =
    new JavaAccessGraph(program, nLogger, idSeed,
      nNodesSet, nUsersMap, nUsesMap,
      nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
      nDominantUsesMap, nDominatedUsesMap,
      nAbstractionsMap, nRecording)

}
