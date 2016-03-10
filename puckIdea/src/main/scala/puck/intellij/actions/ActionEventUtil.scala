/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.intellij.actions

import com.intellij.openapi.actionSystem.{LangDataKeys, CommonDataKeys, AnActionEvent, DataKey}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiFile, PsiElement}

/**
 * Created by Loïc Girault on 29/10/15.
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
