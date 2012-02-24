package elw.vo;

/**
 * POST/PUT encoding of new {@link ScoreTerm} information.
 */
public class ScoreTermInput {
    protected String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    protected double ratio;
    public double getRatio() { return ratio; }
    public void setRatio(double ratio) { this.ratio = ratio; }

    protected int pow;
    public int getPow() { return pow; }
    public void setPow(int pow) { this.pow = pow; }

    public ScoreTermInput() {
        //  default constructor, nothing to do here
    }

    public ScoreTermInput(double ratio, String id, int pow) {
        this.ratio = ratio;
        this.id = id;
        this.pow = pow;
    }
}
