import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

class Main {
	public static void main(String[] args) {
		StringSorter sorter = new StringSorter();
		String moku = "shall we just work mokumoku?";
		String sortedMoku = sorter.dictionaryOrder(new StringBuffer(moku));

		assertEq("just mokumoku? shall we work", sortedMoku);

		System.out.print("Javaって実はインタプリタとコンパイラを両方持ってる珍しい言語です\n");
		System.out.println("(厳密にはインタプリタ（もしくはランタイム）も内部でコンパイルしているのですが..)");
		System.out.println("Java以外で両方持ってる言語は、僕が知っている限りだとSwiftくらいです");
		System.out.println("OOP全盛期を牽引した言語というイメージが強いです");
		System.out.println("あと、心なしかC++と書いてる感覚が似てる..");
		return;
	}

	static <T> void assertEq(T expected, T subject) throws AssertionError {
		boolean is_equal = false;
		if (expected instanceof Object) {
			is_equal = expected.equals(subject);
		} else {
			is_equal = expected == subject;
		}

		if (!is_equal) {
			throw new AssertionError(
					"\n\nassertion failed -----------------\n"
							+ "[expected]\n("
							+ expected
							+ ")\n[subject]\n("
							+ subject
							+ ")\n-------------------------\n");
		}
	}
}

class StringSorter {
	String dictionaryOrder(StringBuffer contents) {
		ArrayList<String> words = new ArrayList<>();
		Collections.addAll(words, contents.toString().split(" "));

		words.sort(new WordComparator());
		return String.join(" ", words);
	}
}

class WordComparator implements Comparator<String> {
	@Override
	public int compare(String w1, String w2) {
		return w1.compareTo(w2);
	}
}
