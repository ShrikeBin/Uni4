public class RBTNode 
{
    public Integer value;
    public RBTNode left;
    public RBTNode right;
    public RBTNode parent; 
    public boolean isRed; // true for red, false for black

    public RBTNode(Integer input)
    {
        value = input;
        left = right = null;
        isRed = true; // new node is red
    }   
}