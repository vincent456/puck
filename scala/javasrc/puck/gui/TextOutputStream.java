package puck.gui;

import java.io.OutputStream;
import javax.swing.JTextArea;

public class TextOutputStream extends OutputStream {
   JTextArea jta;
   
   public TextOutputStream (JTextArea jta) {this.jta = jta; }
   
   @Override
   public void write (int b) { jta.append(String.valueOf((char)b)); }
}
