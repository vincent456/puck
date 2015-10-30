package puck.intellij.actions

import com.intellij.openapi.actionSystem.{LangDataKeys, CommonDataKeys, AnActionEvent, DataKey}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiFile, PsiElement}

/**
 * Created by lorilan on 29/10/15.
 */
object ActionEventUtil {
   def getDataFromContext[T](key : DataKey[T], event : AnActionEvent) : Option[T] =
     Option(key.getData(event.getDataContext))


   def getProject(event : AnActionEvent) : Project =
     CommonDataKeys.PROJECT.getData(event.getDataContext)

   def getPsiElement(event : AnActionEvent) : Option[PsiElement] =
     getDataFromContext(CommonDataKeys.PSI_ELEMENT, event)

 //  def getEditor(event : AnActionEvent) : Editor =
 //    CommonDataKeys.EDITOR.getData(event.getDataContext)
 //
   def getPsiFile(event : AnActionEvent) : Option[PsiFile] =
    getDataFromContext(CommonDataKeys.PSI_FILE, event)

   def getVirtualFile(event : AnActionEvent) : Option[VirtualFile] =
     getDataFromContext(CommonDataKeys.VIRTUAL_FILE, event)

   def getModule(event : AnActionEvent) : Option[Module] =
     getDataFromContext(LangDataKeys.MODULE, event)
 }
