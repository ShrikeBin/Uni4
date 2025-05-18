import java.util.*;

public class ExperimentRunner 
{
    public static void runFullExperiment(String treeType, Integer tries) 
    {
        int[] sizes = {10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000};

        for (int n : sizes) 
        {
            System.out.println("[" + treeType + "] n = " + n);
        
            for (int test = 1; test <= 2; test++) 
            {
                long totalCmp = 0, totalRead = 0, totalWrite = 0, totalHeight = 0;
                long totalCost = 0, maxCost = 0;
                int opCount = 0;
        
                for (int i = 0; i < tries; i++) 
                {
                    List<Integer> keys = new ArrayList<>();
                    for (int j = 1; j <= n; j++) keys.add(j);
        
                    if (test == 2) Collections.shuffle(keys); // random insert for Test 2
        
                    TREE tree = createTree(treeType);
                    Metrics metrics = new Metrics();
                    tree.setMetrics(metrics);
        
                    long prevCmp = 0, prevRead = 0, prevWrite = 0;
        
                    // INSERT phase
                    for (int key : keys) 
                    {
                        tree.addNode(key);
                        long cost = (metrics.comparisons - prevCmp)
                                  + (metrics.pointerReads - prevRead)
                                  + (metrics.pointerWrites - prevWrite);
        
                        totalCost += cost;
                        maxCost = Math.max(maxCost, cost);
                        opCount++;
        
                        prevCmp = metrics.comparisons;
                        prevRead = metrics.pointerReads;
                        prevWrite = metrics.pointerWrites;
                    }
        
                    Collections.shuffle(keys); // always random delete
        
                    // DELETE phase
                    for (int key : keys) 
                    {
                        tree.deleteNode(key);
                        long cost = (metrics.comparisons - prevCmp)
                                  + (metrics.pointerReads - prevRead)
                                  + (metrics.pointerWrites - prevWrite);
        
                        totalCost += cost;
                        maxCost = Math.max(maxCost, cost);
                        opCount++;
        
                        prevCmp = metrics.comparisons;
                        prevRead = metrics.pointerReads;
                        prevWrite = metrics.pointerWrites;
                    }
        
                    totalCmp += metrics.comparisons;
                    totalRead += metrics.pointerReads;
                    totalWrite += metrics.pointerWrites;
                    totalHeight += metrics.maxHeight;
                }
        
                String testName = (test == 1)
                    ? "Test 1: Insert sorted, delete random"
                    : "Test 2: Insert random, delete random";
        
                System.out.println(testName + " ran " + tries + " times.");
                System.out.println("Avg comparisons: " + (totalCmp / tries));
                System.out.println("Avg pointer reads: " + (totalRead / tries));
                System.out.println("Avg pointer writes: " + (totalWrite / tries));
                System.out.println("Avg max height: " + (totalHeight / tries));
                System.out.println("Avg operation cost: " + (totalCost / opCount));
                System.out.println("Max operation cost: " + maxCost);
                System.out.println();
            }
        }
    }        

    public static TREE createTree(String type) 
    {
        switch (type) {
            case "BinaryTree": return new BinaryTree();
            case "SplayTree": return new SplayTree();
            case "RBT": return new RBT();
            default: throw new IllegalArgumentException("Unknown tree type: " + type);
    }
}

}
