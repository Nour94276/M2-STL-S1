package kmp;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

public class AppTest {
        private static List<Data> ListData = new ArrayList<>(50);

        @ParameterizedTest
        @CsvSource({ "The Foundation ,All that happened in a week.txt,1",
                        "DONATIONS or determine ,All that happened in a week.txt,2",
                        "God-fearing folk on the Sabbath , The Project Gutenberg.txt,3",
                        "to slip away to play, The Project Gutenberg.txt,4",
                        "They bethought themselves,Roman pictures.txt,5",
                        "said Teresa,Roman pictures.txt,6",
                        "if he is not prepared,Roman pictures.txt,7",
                        "take the evening ,Roman pictures.txt,8" })
        public void TestsDePerformance(String Inputext, String LivreFile, int ID) {
                // Appel à votre méthode pour obtenir les données Data
                Data data = App.searchInLocalFile(Inputext, LivreFile, ID, "TestsDePerformance");
                ListData.add(data);
                App.WriteInYAMLFile(ListData);
        }

        @ParameterizedTest
        @CsvSource({ "ipsum ,TestCorrespondanceMultiple.txt,1" })
        public void TestMultipleMatches(String Inputext, String LivreFile, int ID) {
                Data data = App.searchInLocalFile(Inputext, LivreFile, ID, "TestMultipleMatches");
                ListData.add(data);
                App.WriteInYAMLFile(ListData);

        }

        @ParameterizedTest
        @CsvSource({ "NoMatch ,TestCorrespondanceMultiple.txt,1" })
        public void TestNoMatch(String Inputext, String LivreFile, int ID) {
                Data data = App.searchInLocalFile(Inputext, LivreFile, ID, "TestNoMatch");
                ListData.add(data);
                App.WriteInYAMLFile(ListData);

        }

        @ParameterizedTest
        @CsvSource({ "a ,All that happened in a week.txt,1" })
        public void TestCasLimiteCourt(String Inputext, String LivreFile, int ID) {
                Data data = App.searchInLocalFile(Inputext, LivreFile, ID, "TestCasLimiteCourt");
                ListData.add(data);
                App.WriteInYAMLFile(ListData);

        }

        @ParameterizedTest
        @ValueSource(strings = {
                        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam euismod purus in mauris dignissim cursus. Integer vehicula, odio eget pellentesque suscipit, tellus dolor euismod enim, non tincidunt arcu justo id odio. Aliquam erat volutpat. Sed laoreet urna in nunc sodales, nec vulputate lectus auctor. Integer commodo, sapien a ullamcorper dictum, dui libero dictum ex, a efficitur metus urna vel lorem. Nunc sed dapibus odio. Quisque tincidunt neque ut bibendum blandit. Sed in sapien nec ligula sollicitudin mattis. Etiam sit amet justo vel erat blandit vestibulum nec vel ligula. Morbi auctor, elit sit amet mattis rhoncus, metus velit fermentum erat, ut lacinia risus dui auctor purus. Aenean at venenatis tellus. Integer mattis interdum purus id facilisis. Phasellus auctor mi ut lectus dapibus, ac hendrerit quam bibendum. Cras dictum arcu at ligula malesuada, nec aliquam justo varius. Suspendisse potenti.Fusce vel augue vitae ipsum volutpat tincidunt vel in enim. Aenean sagittis at odio at semper. Vivamus a ipsum a neque aliquet vestibulum. Fusce volutpat quam ut lorem euismod rhoncus. Quisque efficitur, libero ut consequat hendrerit, purus ex bibendum erat, ac blandit ligula nulla ac justo. Fusce iaculis vel est eu sagittis. Aliquam at scelerisque arcu. Integer in malesuada nunc, sit amet posuere arcu. Sed eu varius ipsum. Donec ac ex hendrerit, viverra velit ut, vehicula erat. Curabitur quis mi eget enim euismod commodo. Sed semper efficitur facilisis. Aenean nec ligula in enim luctus fringilla. Etiam efficitur odio nec erat scelerisque, a iaculis justo interdum. Vivamus quis dui ac velit tristique bibendum. Curabitur vel nisl ut justo facilisis dignissim. Pellentesque sodales mi eu augue aliquet, nec dapibus enim viverra. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Donec sodales interdum massa, eget ullamcorper augue congue at. Vestibulum sed libero vitae arcu congue vulputate. Integer vehicula ultrices libero, eget elementum odio auctor nec. Integer non tortor et justo facilisis suscipit. Suspendisse facilisis dolor et turpis sollicitudin congue. Vivamus aliquam, dolor a consectetur rhoncus, est dolor semper neque, sit amet interdum ligula sapien id risus. Integer tristique nulla nec est posuere, et commodo libero vulputate. Vivamus vehicula odio auctor, viverra leo ut, fringilla ipsum. Vivamus a mi nec erat interdum tincidunt ac at turpis. Vivamus vel pharetra lorem. Duis facilisis interdum bibendum. Sed elementum bibendum orci vel tristique. Sed vestibulum, arcu non venenatis interdum, erat odio ultrices elit, nec vestibulum urna metus in justo. Integer viverra sed sapien quis malesuada. Maecenas sit amet nisl a est vulputate facilisis. Sed aliquet, lorem a congue faucibus, tortor urna iaculis libero, ac mattis tellus felis id odio. Cras vulputate odio a libero efficitur, vel euismod augue facilisis.Morbi non rhoncus libero, sit amet faucibus purus. Nulla facilisi. Vivamus eu bibendum purus. Fusce rhoncus efficitur odio, ac sodales est euismod in. Duis sagittis, justo non tempus ultricies, libero orci gravida turpis, in semper nulla tortor id odio. Sed eu dolor quis urna tincidunt volutpat. Curabitur ac ex nec ipsum egestas congue. Donec vel justo vitae urna tempor efficitur. Donec non libero at nunc elementum tincidunt vel non ex. Curabitur non dui quis justo ullamcorper dictum eget nec ligula. Nullam vitae arcu vel eros euismod tempus. Sed et quam vel dolor pharetra scelerisque.Phasellus efficitur velit sed magna feugiat volutpat. Aliquam eget tortor id odio laoreet laoreet non sed dui. Sed interdum " })
        public void TestCasLimiteLong(String Inputext) {
                Data data = App.searchInLocalFile(Inputext, "TestCasLimiteLong.txt", 1, "TestCasLimiteLong");
                ListData.add(data);
                App.WriteInYAMLFile(ListData);

        }
}
