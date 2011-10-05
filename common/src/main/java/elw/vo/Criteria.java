package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.Map;

public class Criteria implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private String powDef = "";
    public String getPowDef() { return powDef; }
    public void setPowDef(String powDef) { this.powDef = powDef; }

    private int powMax = 1;
    public int getPowMax() { return powMax; }
    public void setPowMax(int powMax) { this.powMax = powMax; }

    private String ratio = "1.0";
    public String getRatio() { return ratio; }
    public void setRatio(String ratio) { this.ratio = ratio; }

    public boolean auto() {
        return (powDef != null && powDef.contains("$")) || (ratio != null && ratio.contains("$"));
    }

    public Integer resolvePowDef(Map<String, Double> vars) {
        if (powDef.startsWith("$")) {
            if (vars != null) {
                for (String var : vars.keySet()) {
                    if (powDef.trim().equalsIgnoreCase(var)) {
                        return vars.get(var).intValue();
                    }
                }
            }

            return null;
        }

        return Integer.parseInt(powDef);
    }

    public Double resolveRatio(Map<String, Double> vars) {
        if (ratio.startsWith("$")) {
            if (vars != null) {
                for (String var : vars.keySet()) {
                    if (ratio.trim().equalsIgnoreCase(var)) {
                        return vars.get(var);
                    }
                }
            }

            return null;
        }

        return Double.parseDouble(ratio);
    }

    @JsonIgnore
    public int[] getPows() {
        final int[] pows = new int[powMax + 1];

        for (int i = 0; i < pows.length; i++) {
            pows[i] = i;
        }

        return pows;
    }
}
