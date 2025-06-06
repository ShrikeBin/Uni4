import java.util.ArrayList;
import java.util.List;

public class BinaryTree implements TREE 
{
    private Node root = null;
    private Metrics metrics;
    BinaryTree(int data){addNode(data);}
    BinaryTree(){root = null;}

    @Override
    public void setMetrics(Metrics m) {this.metrics = m;}
    @Override
    public Metrics getMetrics() {return this.metrics;}

    @Override
    public void addNode(int data) 
    {
        root = insertRecursive(root, data);
    }

    private Node insertRecursive(Node current, int data) 
    {
        if (current == null) 
        {   
            Node node = new Node(data);
            return node;
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

    private Node deleteRecursive(Node current, int data) 
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

    private int findMin(Node node) 
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
        return heightRecursive(root);
    }

    private int heightRecursive(Node node) 
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

    private void inOrderTraversal(Node node) 
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

    private void printRec(Node node, String prefix, boolean isTail, boolean isLeft, boolean isFirst, List<String> lines) 
    {
        if (node == null) return;

        if(!isFirst)
        {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + node.value);
        }
        else
        {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + node.value);
        }
        List<Node> children = new ArrayList<>();
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
            printRec(children.get(i), newPrefix, childIsTail, isLeftList.get(i), false, lines);
        }
    }
}