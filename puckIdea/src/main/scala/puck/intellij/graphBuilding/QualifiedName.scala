package puck.intellij.graphBuilding

import com.intellij.psi._
/**
 * Created by lorilan on 22/10/15.
 */
object Name{
  def apply(t : PsiType) : String = t match {
    case pct : PsiClassType => pct.resolve().getName
    case ppt : PsiPrimitiveType => ppt.getCanonicalText(false)
    case pat : PsiArrayType =>
      val n = Name(pat.getComponentType)
      s"$n[]"
    case _ =>
      sys.error(s"Name.apply(t : ${t.getClass}) unhandled case")
  }
}


object QualifiedName {

  def apply(aClass : PsiClass) : String = aClass.getQualifiedName

  def memberQN(member : PsiMember) =
    member.getContainingClass.getQualifiedName + "." + member.getName

  def apply(field : PsiField) : String =
    memberQN(field)

  def apply(method : PsiMethod) : String = {
    val pml = method.getParameterList.getParameters.toList
    val argsStr = pml map (p => Name(p.getType)) mkString ("__", "_", "")
    memberQN(method) + argsStr
  }

  def packageName(topLevelClass : PsiClass) : String = {
    val jf = topLevelClass.getContainingFile.asInstanceOf[PsiJavaFile]
    jf.getPackageName
  }
}
