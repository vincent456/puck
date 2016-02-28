package p;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
interface AttributeTableModel extends TableModel {
    int getRowCount();
}

abstract class AttributeTableModelDecoratorAdapter extends AbstractTableModel implements AttributeTableModel {
}
public class View {

    private AttributeTableModelDecoratorAdapter currentAttributeTableModel;

    private void startEditingTable() {
        currentAttributeTableModel.getRowCount();
    }

}

//interface TableModel {
//    int m();
//}
//abstract class AbstractTableModel implements TableModel {}
//interface AttributeTableModel extends TableModel {
//    int m();
//}
//abstract class C extends AbstractTableModel implements AttributeTableModel {
//}
//
//class D {
//    C c;
//    void n(){
//        c.m();
//    }
//}
