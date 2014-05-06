package puck.graph;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import puck.graph.edges.AGEdge;
import puck.graph.nodes.AGNode;
import puck.graph.nodes.NodeFactory;
import puck.graph.nodes.NullNode;
import AST.Access;
import AST.BodyDecl;
import AST.ClassDecl;
import AST.CompilationUnit;
import AST.ConstructorDecl;
import AST.FieldDecl;
import AST.FieldDeclaration;
import AST.InstanceInitializer;
import AST.InterfaceDecl;
import AST.MemberClassDecl;
import AST.MethodDecl;
import AST.ParTypeAccess;
import AST.ParameterDeclaration;
import AST.Program;
import AST.StaticInitializer;
import AST.TypeAccess;
import AST.TypeDecl;
import AST.VarAccess;
import AST.Variable;
import AST.VariableDecl;
import AST.VariableDeclaration;

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

	public boolean addCompilationUnit(CompilationUnit c){
		if(c.pathName() == null)
			return false;

		//one compilation unit = 1 .java or 1 .class
		//System.out.println(c.pathName());

		AGNode packageNode = ag.addPackageDecl(c.getPackageDecl()); 

		for (TypeDecl td : c.getTypeDecls()) {
			packageNode.addContains(addTypeDecl(td));
		}
		return true;
	}
	
	AGNode getBodyDeclNode(BodyDecl bd){

		//replace with bd.buildAGNode(builder)

		// if (bd instanceof FieldDeclaration ){
		// 	return ag.addFieldNode((FieldDeclaration) bd);
		// }

		// if( bd instanceof MethodDecl){
		// 	return ag.addMethodNode((MethodDecl)bd);
		// }

		// if( bd instanceof ConstructorDecl){
		// 	return ag.addConstructorNode((ConstructorDecl)bd);
		// }

		// if(bd instanceof FieldDecl){
		// 	return NodeFactory.createNode(ag.getId(), (FieldDecl)bd);
		// }

		// if(bd instanceof InstanceInitializer
		// 		|| bd instanceof StaticInitializer)
		// 	return ag.addTypeNode(bd.hostType());


		// return NullNode.instance;

	}


	private AGNode addApiTypeNode(TypeDecl td){

		AGNode packageNode = ag.addPackageDecl(td.compilationUnit().getPackageDecl()); 
		AGNode tdNode = null; 
		System.out.println("added "+ packageNode.fullName());

		if (td instanceof ClassDecl) {
			tdNode = ag.addClassNode((ClassDecl)td);
		} else if (td instanceof InterfaceDecl) {
			tdNode = ag.addInterfaceNode((InterfaceDecl)td);
		}

		for (Access use : td.uses()) {
			getBodyDeclNode(use.hostBodyDecl()).addUse(tdNode);
		}

		packageNode.addContains(tdNode);
		System.out.println("added "+ tdNode.fullName());
		return tdNode;
	}

	/**
	 * Check if the type declaration is a Class Declaration or an Interface declaration
	 * and add it to the corresponding set
	 *  
	 * @param td
	 * @param containerNode
	 * @param bd_filter 
	 */
	private AGNode addTypeDecl(TypeDecl td){
		AGNode tdNode = null; 

		/*if (td instanceof ClassDecl) {
			tdNode = addClassDecl((ClassDecl)td);
		} else if (td instanceof InterfaceDecl) {
			tdNode = addInterfaceDecl((InterfaceDecl)td);
		}*/

		// for (Access use : td.uses()) {
		// 	getBodyDeclNode(use.hostBodyDecl()).addUse(tdNode);
		// }

		for (BodyDecl bd : td.getBodyDeclList()) {
			AGNode bdNode = addBodyDeclAndUses(bd, tdNode);

			//if bd is an initializer (static or not), 
			//the corresponding AGNode is tdNode !!
			if(bdNode!=tdNode)
				tdNode.addContains(bdNode);
		}
		return tdNode;
	}


	private void addMethodUsesDependencies(Access methodUse, AGNode user, AGNode methodNode, AGNode typehostAGNode){
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
	}


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
	private void addStringLiteral(String literal, Collection<BodyDecl> occurences){

		System.out.println("string "+literal + " "+ occurences.size()+" occurences" );
		for (BodyDecl bd : occurences) {
			CompilationUnit c = bd.hostBodyDecl().compilationUnit();

			//nécessaire ? sera forcément fait via addPackageNode ?
			AGNode packageNode = ag.addPackageDecl(c.getPackageDecl());

			AGNode bdNode = getBodyDeclNode(bd);
			AGNode slNode = ag.addStringNode(literal, 
					NodeFactory.packageNameFilter(bd.hostPackage()));

			packageNode.addContains(slNode);
			bdNode.addUse(slNode);
		}
	}

	private static ConstructorDecl findConstructorBySignature(TypeDecl td, String paramsDescr){
		Collection<ConstructorDecl> crs = td.constructors();
		for(ConstructorDecl c : crs){
			if(c.getParametersAsString().equals(paramsDescr)){
				return c;
			}
		}
		return null;
	}


	public void addApiNode(String nodeKind, String type, String bodydeclName){
		System.out.println("trying to add type "+type + " " + bodydeclName+ " ... ");
		TypeDecl td = program.findType(type);
		if(td == null){
			System.out.println(type + " not found");
			return;
		}

		AGNode tdNode = addApiTypeNode(td);
		BodyDecl bd = null;

		if(nodeKind.equals("type")){
			return;
		}
		else if(nodeKind.equals("method")){
			bd = td.findMethodBySig(bodydeclName);
		}
		else if(nodeKind.equals("constructor")){
			bd = findConstructorBySignature(td, "#_"+bodydeclName);
		}
		else{
			System.err.println("node kind unknown");
			return;
		}

		if(bd == null){
			System.out.println("Method or constructor" + bodydeclName + " not found in the program ...");
			return;
		}
		tdNode.addContains(addBodyDeclAndUses(bd, tdNode));

//		System.out.println(tdNode.fullName() + " contains ");
//		for(AGNode n : tdNode.getContains()){
//			System.out.println(n.fullName());
//		}
	}



}
