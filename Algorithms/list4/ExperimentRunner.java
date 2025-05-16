import java.util.*;

public class ExperimentRunner 
{
    public static void runFullExperiment(String treeType) 
    {
        int[] sizes = {10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000};

        for (int n : sizes) {
            System.out.println("[" + treeType + "] n = " + n);

            // Test 1: Insert sorted, delete random
            long cmp1 = 0, read1 = 0, write1 = 0;
            int height1 = 0;

            // Test 2: Insert random, delete random
            long cmp2 = 0, read2 = 0, write2 = 0;
            int height2 = 0;

            for (int i = 0; i < 20; i++) {
                List<Integer> keys = new ArrayList<>();
                for (int j = 1; j <= n; j++) keys.add(j);

                // --- TEST 1: insert sorted, delete random ---
                TREE tree1 = createTree(treeType);
                Metrics m1 = new Metrics();
                tree1.setMetrics(m1);

                for (int key : keys) tree1.addNode(key);
                Collections.shuffle(keys);
                for (int key : keys) tree1.deleteNode(key);

                cmp1 += m1.comparisons;
                read1 += m1.pointerReads;
                write1 += m1.pointerWrites;
                height1 += m1.maxHeight;

                // --- TEST 2: insert random, delete random ---
                Collections.shuffle(keys);
                TREE tree2 = createTree(treeType);
                Metrics m2 = new Metrics();
                tree2.setMetrics(m2);

                for (int key : keys) tree2.addNode(key);
                Collections.shuffle(keys);
                for (int key : keys) tree2.deleteNode(key);

                cmp2 += m2.comparisons;
                read2 += m2.pointerReads;
                write2 += m2.pointerWrites;
                height2 += m2.maxHeight;
            }

            System.out.println("Test 1: Insert sorted, delete random");
            System.out.println("Avg comparisons: " + (cmp1 / 20));
            System.out.println("Avg pointer reads: " + (read1 / 20));
            System.out.println("Avg pointer writes: " + (write1 / 20));
            System.out.println("Avg max height: " + (height1 / 20));

            System.out.println("Test 2: Insert random, delete random");
            System.out.println("Avg comparisons: " + (cmp2 / 20));
            System.out.println("Avg pointer reads: " + (read2 / 20));
            System.out.println("Avg pointer writes: " + (write2 / 20));
            System.out.println("Avg max height: " + (height2 / 20));

            System.out.println();
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
