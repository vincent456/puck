package location;

import java.util.ArrayList;
import java.util.List;

public class Location {
	private List<Velo> velos = new ArrayList<Velo>();
	public Location(int n) {
		for (int i = 0; i < n; i++) {
		velos.add(new Velo(i));}
	} 
    public Velo louer(String user) {
    	Velo dispo = null;
    	for (Velo v : velos) {
    		if (v.getUtilisateur() == null) {
    			dispo = v;
    			dispo.setUtilisateur(user);
    			break;
    		}
    	}
    	return dispo;
    }
    public int getNombreVelos() {return velos.size();}
 
    public void rendreVelo(int id) { 
    	for (Velo v : velos) {
    		if (v.getId() == id) {
    			v.setUtilisateur(null);
    			break;
    		}
    	}
    }
    public int getNombreVeloLibres() {
    	int dispo = 0;
    	for (Velo v : velos) {
    		if (v.getUtilisateur() == null) {
    			dispo++;   	
    		}
    	}
    	return dispo;
    }    
}