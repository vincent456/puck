package puck.graph;

import puck.graph.edges.AGEdge;
import puck.graph.nodes.AGNode;
import puck.graph.nodes.NodeFactory;
import puck.graph.nodes.NullNode;


public class Builder {

	

	private AccessGraph ag;
	private Program program;

	public Builder(){}

	public AccessGraph build(Program p, Map<String,Collection<BodyDecl>> allStringUses, LoadingListener ll){
		ag = new AccessGraph();
		program = p;
		

		if(allStringUses != null){
			allStringUses = p.AllStringUses(allStringUses);
			for (Entry<String, Collection<BodyDecl>>  entry : allStringUses.entrySet()) {
				addStringLiteral(entry.getKey(), entry.getValue());
			}
		}

		if(ll == null)
			silentBuild();
		else
			verboseBuild(ll);

		ag.ensureContainers(); 
		ag.computeRoots();

		return ag;
	}

	/**
	 * For a given compilation unit, add the corresponding package node and then
	 * add all the contained TypeDeclaration
	 * @param c
	 * @param bd_filter
	 * @return
	 */

	// AGNode getBodyDeclNode(BodyDecl bd){
		//replace with bd.buildAGNode(builder)

	//private AGNode addTypeDecl(TypeDecl td){
	//  td.buildAG(builder)


	private void addMethodUsesDependencies(Access methodUse, AGNode user, AGNode methodNode, AGNode typehostAGNode){
		//methodUse.getQualifier().
		if(methodUse.getQualifier() instanceof VarAccess){
			Variable varUser = ((VarAccess) methodUse.getQualifier()).decl();
			AGNode bodyDeclAGNode;
			if(varUser instanceof FieldDeclaration){
				bodyDeclAGNode = getBodyDeclNode((FieldDeclaration)varUser);
			}
			else if(varUser instanceof VariableDeclaration){
				bodyDeclAGNode = getBodyDeclNode(((VariableDeclaration)varUser).hostBodyDecl());
			}
			else{
				bodyDeclAGNode = getBodyDeclNode(((ParameterDeclaration)varUser).hostBodyDecl());
			}
			
			AGEdge methodUses = AGEdge.createUsesEdge(user, methodNode);
			AGEdge methodReceiverTypeDeclaration = AGEdge.createUsesEdge(bodyDeclAGNode, typehostAGNode);

			ag.addUsesDependency(methodUses, methodReceiverTypeDeclaration);
		}
		else{
			System.out.println("Method uses dependencie not treated, qualifier : " + methodUse.getQualifier());
		}
	}
	
	//private AGNode addBodyDeclAndUses(BodyDecl bd, AGNode typehostAGNode){
		// use bd.buildAG(builder); no need for typehostAGNode
	
	//private AGNode addBodyDecl(BodyDecl bd, AGNode usedNode){
		// use bd.buildAG(builder, usedNode); instead



	/**
	 * 
	 * Create the Graph nodes representing a literal string and the corresponding 
	 * use edges from the description of the string and all the body decleration 
	 * where it appears
	 *  
	 * 
	 * @param literal
	 * @param occurences : all body declaration where the literal string appears
	 */
	

	


	



}
