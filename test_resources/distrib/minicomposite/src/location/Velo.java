package location;

public class Velo {

	private int id;

	private String utilisateur = null;
    
    public Velo(int i) {
		this.id = i;
	}
	    public void setUtilisateur(String u) {
    	this.utilisateur = u;
    }

	public String getUtilisateur() {return utilisateur;}

	public void rendre() {setUtilisateur(null);}

	public int getId() {return id;}

}