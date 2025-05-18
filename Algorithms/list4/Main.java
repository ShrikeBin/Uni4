import java.util.Scanner;

public class Main 
{
    public static void main(String[] args) 
    {
        if(args.length > 0)
        {
            if(args[0].equals("exp"))
            {   
                System.out.println("running simulations...");

                System.out.println("running binary tree: ");
                //ExperimentRunner.runFullExperiment("BinaryTree", 20);

                System.out.println("running splay tree: ");
                ExperimentRunner.runFullExperiment("SplayTree", 20);

                System.out.println("running RB tree: ");
                ExperimentRunner.runFullExperiment("RBT", 20);
            }
        }
        else
        {
            Scanner scanner = new Scanner(System.in);
            System.out.print("Enter Tree type (e.g. Binary, RBT, Splay, ...): ");
            String typeInput = scanner.nextLine().trim();

            switch (typeInput.toLowerCase())
            {
                case "binary":
                case "bin":
                    CLI binaryCLI = new CLI("binary");
                    binaryCLI.run();
                    break;
                case "splay":
                    CLI splayCLI = new CLI("splay");
                    splayCLI.run();
                    break;
                case "redblack":
                case "rbt":
                    CLI redBlackCLI = new CLI("redblack");
                    redBlackCLI.run();
                    break;
                default:
                    System.out.println("Unknown Type, closing....");
                    break;
            }
            scanner.close();
        } 
    }
}