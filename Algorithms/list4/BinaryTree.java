import java.util.ArrayList;
import java.util.List;

public class BinaryTree implements TREE
{
    private Node root;

    public BinaryTree(Node root) 
    {
        this.root = root;
    }

    @Override
    public void addNode(Integer parentData) 
    {
        Node input = new Node(parentData);

        if (root == null) 
        {
            this.root = input;
        } 
        else 
        {
            Node prev = null;
            Node temp = root;

            while (temp != null) 
            {
                prev = temp;
                if (temp.getStem() > parentData) 
                {
                    temp = temp.getLeft();
                } 
                else if (temp.getStem() < parentData) 
                {
                    temp = temp.getRight();
                } 
                else 
                {
                    return; // duplicate
                }
            }

            if (prev.getStem() > parentData) 
            {
                prev.setLeft(input);
            } 
            else 
            {
                prev.setRight(input);
            }
        }
    }

    @Override
    public void deleteNode(Integer keyData) 
    {
        this.root = deleteAsBaseOn(this.root, keyData);
    }

    private Node deleteAsBaseOn(Node root, Integer keyData) 
    {
        if (root == null) 
        {
            return root;
        }

        if (keyData < root.getStem()) 
        {
            root.setLeft(deleteAsBaseOn(root.getLeft(), keyData));
        } 
        else if (keyData > root.getStem()) 
        {
            root.setRight(deleteAsBaseOn(root.getRight(), keyData));
        } 
        else 
        {
            // Node with only one child or no child
            if (root.getLeft() == null) 
            {
                return root.getRight();
            } 
            else if (root.getRight() == null) 
            {
                return root.getLeft();
            }

            // Node with two children
            root.setStem(smallestLeftTree(root.getRight()));
            root.setRight(deleteAsBaseOn(root.getRight(), root.getStem()));
        }

        return root;
    }

    @Override
    public int getHeight() 
    {
        return heightHelper(root);
    }

    private int heightHelper(Node node) 
    {
        if (node == null) 
        {
            return -1;
        }

        int leftHeight = heightHelper(node.getLeft());
        int rightHeight = heightHelper(node.getRight());

        return Math.max(leftHeight, rightHeight) + 1;
    }

    private Integer smallestLeftTree(Node root) 
    {
        Integer min = root.getStem();
        while (root.getLeft() != null) 
        {
            min = root.getLeft().getStem();
            root = root.getLeft();
        }
        return min;
    }
    
    @Override
    public void printTreeOrder() 
    {
        preorderTraversal(root);
        System.out.print("[END] \n");
    }

    private void preorderTraversal(Node core) 
    {
        if (core == null) 
        {
            return;
        }
        System.out.print(core.getStem() + " - ");
        preorderTraversal(core.getLeft());
        preorderTraversal(core.getRight());
    }

    @Override
    public void printTreeFull() 
    {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, lines);
        for(String line : lines)
        {
            System.out.println(line);
        }
    }

    private void printRec(Node node, String prefix, boolean isTail, List<String> lines) 
    {
        if (node == null) 
        {
            return;
        }

        lines.add(prefix + (isTail ? "└── " : "├── ") + node.getStem());

        List<Node> children = new ArrayList<>();
        if (node.getLeft() != null) 
        {
            children.add(node.getLeft());
        }
        if (node.getRight() != null) 
        {
            children.add(node.getRight());
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
