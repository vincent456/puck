package p1;

import java.util.List;

public interface TextTranslator{}

public class ComboProperty {

    public ComboProperty(String[] possibles, TextTranslator pTranslator) {
		super();
    }

    public ComboProperty(String[] possibles, List possibleTranslations) {}

	public ComboProperty(List possibles, List possibleTranslations) {}
}
