public class GetProperty {
	public static void main(String... args) {
		for (String arg : args) {
			System.out.println(System.getProperty(arg));
		}
	}
}
