package puck.gui.svg;

import puck.graph.NodeKind;
import puck.graph.transformations.CreateVarStrategy;
import puck.graph.transformations.CreateVarStrategyForJava;

import javax.swing.*;

/**
 * Created by lorilan on 3/31/15.
 */
public class ParamOrFieldChoice {

    public static CreateVarStrategy getChoice(NodeKind k){
        Object[] choices = {CreateVarStrategyForJava.createParamater(),
                            CreateVarStrategyForJava.createTypeMember(k)};

        return (CreateVarStrategy)
                JOptionPane.showInputDialog(null, //Component parentComponent
                "Parameter or Field ?", //Object message,
                "How to get self reference", //String title
                JOptionPane.PLAIN_MESSAGE, //int messageType
                null, //Icon icon,
                choices, //Object[] options,
                CreateVarStrategyForJava.createTypeMember(k));//Object initialValue


    }

}
