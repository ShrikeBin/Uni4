import java.util.Scanner;

public class Main 
{
    public static void main(String[] args) 
    {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter type T (e.g., Integer, String): ");
        String typeInput = scanner.nextLine().trim();

        switch (typeInput.toLowerCase())
        {
            case "int":
            case "integer":
            {
                CLI<Integer> INT = new CLI<>(new Parser<Integer>() 
                {
                    @Override
                    public Integer parse(String input) 
                    {
                        return Integer.parseInt(input);
                    }
                });

                INT.run();
                break;
            }
            case "string": 
            {
                CLI<String> STRING = new CLI<>(new Parser<String>() {
                    @Override
                    public String parse(String input) 
                    {
                        return input;
                    }
                });
                STRING.run();
                break;
            }
            case "double": 
            {
                CLI<Double> DOUBLE = new CLI<>(new Parser<Double>() {
                    @Override
                    public Double parse(String input) 
                    {
                        return Double.parseDouble(input);
                    }
                });
                DOUBLE.run();
                break;
            }
            default:
                System.out.println("Unknown Type, closing....");
                break;
        }
        scanner.close(); 
    }
}