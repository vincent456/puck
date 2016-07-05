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

package puck;

import org.extendj.ast.JavaJastAddDG2AST;
import org.extendj.ast.JavaJastAddDG2AST$;
import puck.graph.io.CSVPrinter$;
import puck.util.FileHelper$;
import puck.util.PuckNoopLogger$;
import scala.collection.JavaConversions$;

import java.io.File;
import java.util.*;


/**
 * Created by Loïc Girault on 05/07/16.
 */
public class AccessFromJavaExample {

    public static void main(String[] args){

        scala.collection.Iterator<String> stringEmptyIterator =
                JavaConversions$.MODULE$.asScalaIterator(Collections.emptyIterator());

        scala.collection.Iterator<scala.Tuple2<String, String>> tuple2emptyIterator =
                JavaConversions$.MODULE$.asScalaIterator(Collections.emptyIterator());

        scala.collection.Iterator<File> fileEmptyIterator =
                JavaConversions$.MODULE$.asScalaIterator(Collections.emptyIterator());

//        scala.collection.Iterator<String> fileFullPaths =
//                JavaConversions$.MODULE$.asScalaIterator(Arrays.asList(args).iterator()).toList();

        scala.collection.immutable.List<String> fileFullPaths = FileHelper$.MODULE$.findAllFiles(
                new File("/home/lorilan/projects/constraintsSolver/test_resources/distrib/bridge/hannemann_simplified"),
                ".java", fileEmptyIterator.toSeq());
        JavaJastAddDG2AST dg2ast =
                JavaJastAddDG2AST$.MODULE$.fromFiles(fileFullPaths,
                        stringEmptyIterator.toList(),
                        stringEmptyIterator.toList(),
                        stringEmptyIterator.toList(),
                        tuple2emptyIterator.toList(), null, PuckNoopLogger$.MODULE$);

        File dir = new File("/tmp/out");
        if(!dir.exists())
            dir.mkdir();

        CSVPrinter$.MODULE$.apply(dg2ast.initialGraph(), dir, ";");
    }
}

