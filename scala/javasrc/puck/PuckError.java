package puck;

public class PuckError extends Error{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public PuckError(){
		super();
	}
	public PuckError(String msg){
		super(msg);
	}
}
