package puck.gui;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;



class ImageComponent extends JComponent{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Image image;
    public ImageComponent(Image img){
        image = img;
    }
    public void paintComponent (Graphics g){
        if(image == null) return;
        g.drawImage(image, 0, 0, this);
    }

}

class ImageFrame extends JFrame{
	
    private Image image;

	public ImageFrame(String imgFileName){
        try{
            setTitle("");
            setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            
            File imgFile = new File(imgFileName);
            
            if(! imgFile.exists()){
            	throw new ImageFrameError(imgFile.toString()+" not found");
            }
            
            image = ImageIO.read(imgFile);
            if(image == null){
            	throw new ImageFrameError("cannot read "+imgFile);
            }

            /*JScrollPane scrollPane = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
            									ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            
            ImageComponent imgCpt = new ImageComponent(image);
        	
            imgCpt.setSize(new Dimension(image.getWidth(this),  image.getHeight(this)));
            
            scrollPane.setViewportView(imgCpt);
            scrollPane.setAutoscrolls(true);
        	add(scrollPane);
        	
        	Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        	
        	setSize((int)Math.min(screenSize.getWidth(), image.getWidth(this)),
        			(int)Math.min(screenSize.getHeight(), image.getHeight(this)+50));*/
        	
        	ScrollablePicture picture = new ScrollablePicture(new ImageIcon(image), 2);

        	JScrollPane pictureScrollPane = new JScrollPane(picture);
        	pictureScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        	pictureScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        	add(pictureScrollPane);
        	
        	Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        	
        	setSize((int)Math.min(screenSize.getWidth(), image.getWidth(this)+20),
        			(int)Math.min(screenSize.getHeight(), image.getHeight(this)+70));
        }
        catch (IOException e){
            e.printStackTrace();
        }
        
        


    }
	
}