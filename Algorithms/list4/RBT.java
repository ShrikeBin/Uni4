import java.util.ArrayList;
import java.util.List;

public class RBT implements TREE
{
    private RBTNode root = null;
    private RBTNode TNULL;

    RBT(int data){addNode(data);}
    RBT(){root = null;}

    @Override
    public void addNode(int data) 
    {
        root = insertRecursive(root, data);
        root.isRed = false;
    }

    private RBTNode insertRecursive(RBTNode current, int data) 
    {
        if (current == null) 
        {
            return new RBTNode(data);
        }

        if (data < current.value) 
        {
            current.left = insertRecursive(current.left, data);
        } 
        else if (data > current.value) 
        {
            current.right = insertRecursive(current.right, data);
        }

        return current;
    }

    @Override
    public void deleteNode(int data)
    {
        root = deleteRecursive(root, data);
    }

    private RBTNode deleteRecursive(RBTNode current, int data) 
    {
        if (current == null) return null;

        if (data < current.value)
        {
            current.left = deleteRecursive(current.left, data);
        } 
        else if (data > current.value) 
        {
            current.right = deleteRecursive(current.right, data);
        } 
        else 
        {
            // Node found
            if (current.left == null) return current.right;
            if (current.right == null) return current.left;

            int minValue = findMin(current.right); // Find Successor's Value
            current.value = minValue;
            current.right = deleteRecursive(current.right, minValue);
        }

        return current;
    }

    private int findMin(RBTNode node) 
    {
        while (node.left != null) 
        {
            node = node.left;
        }
        return node.value;
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
        printRec(root, "", true, true, lines); // root assumed to be a left node
        for (String line : lines) 
        {
            System.out.println(line);
        }
    }

    private void printRec(RBTNode node, String prefix, boolean isTail, boolean isLeft, List<String> lines) 
    {
        if (node == null) return;

        String branchColor = isLeft ? "\u001B[96m" : "\u001B[95m";
        lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + (node.isRed ? "\u001B[31m" + node.value + "\u001B[0m" : "\u001B[30m" + node.value + "\u001B[0m"));
        List<RBTNode> children = new ArrayList<>();
        List<Boolean> isLeftList = new ArrayList<>();

        if (node.left != null) 
        {
            children.add(node.left);
            isLeftList.add(true);
        }
        if (node.right != null) 
        {
            children.add(node.right);
            isLeftList.add(false);
        }

        for (int i = 0; i < children.size(); i++) 
        {
            boolean childIsTail = (i == children.size() - 1);
            String newPrefix = prefix + (isTail ? "    " : "\u001B[90m" + "│   " + "\u001B[0m");
            printRec(children.get(i), newPrefix, childIsTail, isLeftList.get(i), lines);
        }
    }
}
