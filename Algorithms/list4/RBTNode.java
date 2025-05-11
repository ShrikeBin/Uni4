public class RBTNode 
{
    private Integer stem;
    private RBTNode left;
    private RBTNode right;
    private boolean color; // true for red, false for black

    public RBTNode(Integer input) 
    {
        stem = input;
        left = right = null;
        color = true; // new node is red
    }

    public void setLeft(RBTNode left) 
    {
        this.left = left;
    }

    public void setRight(RBTNode right) 
    {
        this.right = right;
    }

    public RBTNode getLeft() 
    {
        return left;
    }

    public RBTNode getRight() 
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

    public boolean isRed() 
    {
        return color;
    }

    public void setColor(boolean color) 
    {
        this.color = color;
    }
    
}