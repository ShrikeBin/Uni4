import java.util.Scanner;

public class Main 
{
    public static void main(String[] args) 
    {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter Tree type (e.g. Binary, RBT, ...): ");
        String typeInput = scanner.nextLine().trim();

        switch (typeInput.toLowerCase())
        {
            case "binary":
            case "bin":
                CLI binaryCLI = new CLI("binary");
                binaryCLI.run();
                break;
            case "splay":
                // CLI splayCLI = new CLI("splay");
                // splayCLI.run();
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