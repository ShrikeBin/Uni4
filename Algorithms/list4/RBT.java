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
        printRec(root, "", true, lines);
        for (String line : lines) 
        {
            System.out.println(line);
        }
    }

    private void printRec(RBTNode node, String prefix, boolean isTail, List<String> lines) 
    {
        if (node == null) return;

        lines.add(prefix + (isTail ? "└── " : "├── ") + node.value);

        List<RBTNode> children = new ArrayList<>();
        if (node.left != null) 
        {
            children.add(node.left);
        }
        if (node.right != null) 
        {
            children.add(node.right);
        }

        for (int i = 0; i < children.size() - 1; i++) 
        {
            printRec(children.get(i), prefix + (isTail ? "    " : "│   "), false, lines);
        }
        if (!children.isEmpty()) 
        {
            printRec(children.get(children.size() - 1), prefix + (isTail ? "    " : "│   "), true, lines);
        }
    }
}
