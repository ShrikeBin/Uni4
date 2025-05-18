import java.util.ArrayList;
import java.util.List;

public class RBT implements TREE
{
    private RBTNode root = null;
    private static RBTNode TNULL = new RBTNode(Integer.MIN_VALUE);
    private ArrayList<RBTNode> debug = new ArrayList<>();

    RBT(int data)
    {
        TNULL.isRed = false; 
        TNULL.parent = null; 
        TNULL.left = TNULL.right = null;
        addNode(data);
    }

    RBT()
    {
        TNULL.isRed = false; 
        TNULL.parent = null; 
        TNULL.left = TNULL.right = null;
    }

    public void debug() 
    {
        for (RBTNode n : debug) 
        {
            System.out.println("Node: " + formatNode(n));
            System.out.println("  Color : " + (n.isRed ? "Red" : "Black"));
            System.out.println("  Parent: " + formatNode(n.parent));
            System.out.println("  Left  : " + formatNode(n.left));
            System.out.println("  Right : " + formatNode(n.right));
            System.out.println();
        }
    
        System.out.println("ROOT: " + formatNode(root));
        System.out.println("  Color : " + (root.isRed ? "Red" : "Black"));
        System.out.println("  Parent: " + formatNode(root.parent));
        System.out.println("  Left  : " + formatNode(root.left));
        System.out.println("  Right : " + formatNode(root.right));
        System.out.println();
    
        System.out.println("TNULL: " + formatNode(TNULL));
        System.out.println("  Color : " + (TNULL.isRed ? "Red" : "Black"));
        System.out.println("  Parent: " + formatNode(TNULL.parent));
        System.out.println("  Left  : " + formatNode(TNULL.left));
        System.out.println("  Right : " + formatNode(TNULL.right));
        System.out.println();
    }
    
    private String formatNode(RBTNode node) 
    {
        if (node == null) return "null";
        if (node == TNULL) return "TNULL";
        if (node.value == Integer.MIN_VALUE) return "TNULL";
        return String.valueOf(node.value);
    }

    @Override
    public void addNode(int data) 
    {
        RBTNode z = new RBTNode(data);
        // TODO Remove Debug
        debug.add(z);

        RBTNode y = TNULL;
        RBTNode x;
        if(root == null)
        {
            x = TNULL;
        }
        else
        {
            x = root;
        }

        while (x != TNULL) 
        {
            y = x;
            if(z.value < x.value)
            {
                x = x.left;
            }
            else if (z.value > x.value)
            {
                x = x.right;
            }
            else 
            {
                return;
            }    
        }

        if(y == TNULL)
        {
            root = z;
        }
        else if(z.value < y.value)
        {
            y.left = z;
            z.parent = y;
        }
        else if (z.value > y.value)
        {   
            y.right = z;
            z.parent = y;
        }
        else
        {
            return;
        }

        z.left = TNULL;
        z.right = TNULL;
        z.isRed = true;
        fixInsert(z);
    }

    private void fixInsert(RBTNode node) 
    {   
        while(node.parent != null && node.parent.isRed)
        {
            if (node.parent == node.parent.parent.left) 
            {
                RBTNode y = node.parent.parent.right; // uncle
                if (y != null && y.isRed) 
                {
                    // Case 1
                    node.parent.isRed = false;
                    y.isRed = false;
                    node.parent.parent.isRed = true;
                    node = node.parent.parent;
                } 
                else
                {
                    if (node == node.parent.right) 
                    {
                        // Case 2
                        node = node.parent;
                        leftRotate(node);
                    }

                    // Case 3
                    node.parent.isRed = false;
                    node.parent.parent.isRed = true;
                    rightRotate(node.parent.parent);
                }
            } 
            else 
            {
                RBTNode y = node.parent.parent.left; // uncle
                if (y != null && y.isRed) 
                {
                    // Case 1
                    node.parent.isRed = false;
                    y.isRed = false;
                    node.parent.parent.isRed = true;
                    node = node.parent.parent;
                } 
                else
                {
                    if (node == node.parent.left) 
                    {
                        // Case 2
                        node = node.parent;
                        rightRotate(node);
                    }
                    
                    // Case 3
                    node.parent.isRed = false;
                    node.parent.parent.isRed = true;
                    leftRotate(node.parent.parent);
                }
            }
        }
        root.isRed = false;
    }

    @Override
    public void deleteNode(int data) 
    {
        RBTNode z = search(data);
        if(z == TNULL)
        {
            return;
        }

        // TODO Remove Debug
        debug.remove(z);

        RBTNode y = z;
        Boolean y_orig_isRed = y.isRed;
        RBTNode x;

        // case 1
        if (z.left == TNULL)
        {
            x = z.right;
            transplant(z, z.right);
        }
        // case 2
        else if(z.right == TNULL)
        {
            x = z.left;
            transplant(z, z.left);
        }
        // case 3
        else
        {
            y = minimum(z.right);
            y_orig_isRed = y.isRed;
            x = y.right;

            if(y.parent == z)
            {
                x.parent = y;
            }
            else
            {
                transplant(y, y.right);
                y.right = z.right;
                y.right.parent = y;
            }

            transplant(z, y);
            y.left = z.left; 
            y.left.parent = y; 
            y.isRed = z.isRed;
        }

        if (!y_orig_isRed)
        {
            System.out.print("Fixup called on node: " + x.value + "\n");
            delete_fixup(x);
        }
        root.parent = null;
        root.isRed = false;
    }

    private void delete_fixup(RBTNode x)
    {
        // w = x.sibling

        // 1 w is red
        // 2 w is black w.left & w.right are black
        // 3 w is black w.left is red w.right is black
        // 4 w is black w.right is red

        System.out.println("Fixup call: " + formatNode(x));
        System.out.println("  Color : " + (x.isRed ? "Red" : "Black"));
        System.out.println("  Parent: " + formatNode(x.parent));
        System.out.println("  Left  : " + formatNode(x.left));
        System.out.println("  Right : " + formatNode(x.right));
        System.out.println();

        while(x != root && !x.isRed)
        {
            // x is left child
            if(x.parent != null && x == x.parent.left)
            {
                System.out.println(2);
                RBTNode w = x.parent.right;
                // case 1
                if(w.isRed)
                {
                    w.isRed = false;
                    x.parent.isRed = true;
                    leftRotate(x.parent);
                    w = x.parent.right;
                }
                // case 2
                if(!w.left.isRed && !w.right.isRed)
                {
                    w.isRed = true;
                    x = x.parent;
                }
                else
                {
                    // case 3
                    if (!w.right.isRed)
                    {
                        w.left.isRed = false;
                        w.isRed = true;
                        rightRotate(w);
                        w = x.parent.right;
                    }
                    // case 4
                    w.isRed = x.parent.isRed;
                    x.parent.isRed = false;
                    w.right.isRed = false;
                    leftRotate(x.parent);
                    x = root;
                }
            }
            // same but swapped for right child x
            else
            {
                RBTNode w = x.parent.left;
                // case 1
                if(w.isRed)
                {
                    w.isRed = false;
                    x.parent.isRed = true;
                    rightRotate(x.parent);
                    w = x.parent.left;
                }
                // case 2
                if(!w.right.isRed && !w.left.isRed)
                {
                    w.isRed = true;
                    x = x.parent;
                }
                else
                {
                    // case 3
                    if(!w.left.isRed)
                    {
                        w.right.isRed = false;
                        w.isRed = true;
                        leftRotate(w);
                        w = x.parent.left;
                    }
                    // case 4
                    w.isRed = x.parent.isRed;
                    x.parent.isRed = false;
                    w.left.isRed = false;
                    rightRotate(x.parent);
                    x = root;
                }
            }
        }
        x.isRed = false;
    }

    private RBTNode minimum(RBTNode node) 
    {
        while (node.left != TNULL) 
        {
            node = node.left;
        }
        return node;
    }

    private RBTNode search(int data) 
    {
        RBTNode current = root;
    
        while (current != TNULL && current.value != data) 
        {
            if (data < current.value)
            {
                current = current.left;
            } 
            else 
            {
                current = current.right;
            }
        }
    
        return current;
    }
    
    private void transplant(RBTNode x, RBTNode y)
    {
        if(x.parent == null)
        {
            root = y;
        }
        else if(x == x.parent.left)
        {
            x.parent.left = y;
        }
        else
        {
            x.parent.right = y;
        }

        x.parent = y.parent;
    }

    private void leftRotate(RBTNode x)
    {
        RBTNode y = x.right;
        if (y == TNULL)
        {
            return;
        }

        x.right = y.left;

        if(y.left != TNULL)
        {
            y.left.parent = x;
        }

        y.parent = x.parent;

        if(x.parent == null)    // x was a root
        {
            root = y;
        }
        else if (x == x.parent.left)
        {
            x.parent.left = y;
        }
        else
        {
            x.parent.right = y;
        }

        y.left = x;
        x.parent = y;
    }

    private void rightRotate(RBTNode x)
    {
        RBTNode y = x.left;
        if (y == TNULL)
        {
            return;
        }

        x.left = y.right;

        if(y.right != TNULL)
        {
            y.right.parent = x;
        }

        y.parent = x.parent;

        if(x.parent == null)     // x was a root
        {
            root = y;
        }
        else if (x == x.parent.left)
        {
            x.parent.left = y;
        }
        else
        {
            x.parent.right = y;
        }

        y.right = x;
        x.parent = y;
    }

    @Override
    public int getHeight() 
    {
        return heightRecursive(root) - 1; // bo TNULL
    }

    private int heightRecursive(RBTNode node) 
    {
        if (node == null) return 0;
        return 1 + Math.max(heightRecursive(node.left), heightRecursive(node.right));
    }

    @Override
    public void printTreeOrder() 
    {
        System.out.print("[START]: ");
        inOrderTraversal(root);
        System.out.println(" [END]");
    }

    private void inOrderTraversal(RBTNode node) 
    {
        if (node == null) return;
        inOrderTraversal(node.left);
        System.out.print(node.value + " ");
        inOrderTraversal(node.right);
    }
        
    @Override
    public void printTreeFull() 
    {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, true, true, lines); // root assumed to be a left node
        for (String line : lines) 
        {
            System.out.println(line);
        }
    }

    private void printRec(RBTNode node, String prefix, boolean isTail, boolean isLeft, boolean isFirst, List<String> lines) 
    {
        if (node == null || node == TNULL) return;

        if(!isFirst)
        {
            String branchColor = isLeft ? "\u001B[32m" : "\u001B[31m";
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + (node.isRed ? "\u001B[91m" + node.value + "\u001B[0m" : "\u001B[90m" + node.value + "\u001B[0m"));
        }
        else
        {
            String branchColor = isLeft ? "\u001B[32m" : "\u001B[31m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + (node.isRed ? "\u001B[91m" + node.value + "\u001B[0m" : "\u001B[90m" + node.value + "\u001B[0m"));
        }

        List<RBTNode> children = new ArrayList<>();
        List<Boolean> isLeftList = new ArrayList<>();

        if (node.left != TNULL) 
        {
            children.add(node.left);
            isLeftList.add(true);
        }
        if (node.right != TNULL) 
        {
            children.add(node.right);
            isLeftList.add(false);
        }

        for (int i = 0; i < children.size(); i++) 
        {
            boolean childIsTail = (i == children.size() - 1);
            String newPrefix = prefix + (isTail ? "    " : "\u001B[90m" + "│   " + "\u001B[0m");
            printRec(children.get(i), newPrefix, childIsTail, isLeftList.get(i), false, lines);
        }
    }
}
