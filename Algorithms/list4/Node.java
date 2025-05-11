public class Node 
{
    private Integer stem;
    private Node left;
    private Node right;

    Node(Integer input) 
    {
        stem = input;
        left = right = null;
    }

    public void setLeft(Node left) 
    {
        this.left = left;
    }

    public void setRight(Node right) 
    {
        this.right = right;
    }

    public Node getLeft() 
    {
        return left;
    }

    public Node getRight() 
    {
        return right;
    }

    public Integer getStem() 
    {
        return stem;
    }

    public void setStem(Integer input) 
    {
        stem = input;
    }
}