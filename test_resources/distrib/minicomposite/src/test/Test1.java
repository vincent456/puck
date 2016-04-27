package test;

import static org.junit.Assert.*;

import org.junit.Test;

import location.Location;

public class Test1 {
	
	final int nbVelos = 5;
	
	@Test
	public void tester() {
		Location loc = new Location(nbVelos);
		assertEquals(nbVelos,loc.getNombreVelos());
		assertEquals(nbVelos,loc.getNombreVeloLibres());
		loc.louer("Mikal");
		assertEquals(nbVelos,loc.getNombreVelos());
		assertEquals(nbVelos-1,loc.getNombreVeloLibres());
	}
}
