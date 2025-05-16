public interface TREE
{
    public void addNode(int data);
    public void deleteNode(int data);
    public int getHeight();
    public void printTreeOrder();
    public void printTreeFull();

    public void setMetrics(Metrics m);
    public Metrics getMetrics();
}