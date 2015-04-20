package webMarket;

public class Front {
    public static void main(String[] args) {
        WelcomeComputer wc = new WelcomeComputer();
        WelcomeCellPhone wcp = new WelcomeCellPhone();
        DetailArticleCellPhone dacp = new DetailArticleCellPhone();
        DetailArticleComputer dac = new DetailArticleComputer();

        wc.printPage();
        wcp.printPage();
        dacp.printPage();
        dac.printPage();
    }
}
