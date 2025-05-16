import java.util.ArrayList;

public class Metrics {
    public long comparisons = 0;
    public long pointerReads = 0;
    public long pointerWrites = 0;
    public int maxHeight = 0;
    public ArrayList<Integer> heights = new ArrayList<>();

    public void reset() {
        comparisons = 0;
        pointerReads = 0;
        pointerWrites = 0;
        maxHeight = 0;
        heights.clear();
    }
}
