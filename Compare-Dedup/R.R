
install.packages("RecordLinkage")

library(RecordLinkage)

install.packages("tibble")

library(tibble)

tibble(a = 1, b = 1:3)

# quelle: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/geburten-todesfaelle/vornamen-schweiz.assetdetail.5946318.html
NAMES_MALE <- c("Daniel", "Peter", "Thomas", "Hans", "Christian", "Martin", "Andreas", "Michael", "Markus", "Marco", "David", "Patrick", "Stefan", "Walter", "Bruno", "Urs", "René", "Marcel", "Roland", "Werner", "Simon", "Pascal", "Beat", "Marc", "Paul", "Kurt", "Roger", "Manuel", "André", "Josef", "Rolf", "Antonio", "Heinz", "Luca", "Rudolf", "Michel", "Robert", "Nicolas", "Christoph", "Jean", "Samuel", "José", "Adrian", "Ernst", "Lukas", "Mario", "Reto", "Philippe", "Fabian", "Alfred", "Matthias", "Alexander", "Philipp", "Stephan", "Pierre", "Jürg", "Florian", "Benjamin", "Jan", "Anton", "Franz", "Giuseppe", "Johann", "Max", "Kevin", "Fabio", "Roman", "Oliver", "Sandro", "Ulrich", "Tobias", "Dominik", "Fritz", "Alain", "Karl", "Jonas", "Olivier", "Gabriel", "Alexandre", "Francesco", "Eric", "Bernhard", "Claudio", "Raphael", "Felix", "Dario", "Noah", "Claude", "Alessandro", "Albert", "Yves", "Richard", "Carlos", "Erich", "Sven", "Robin", "Ivan", "Roberto", "Remo", "Jakob")
NAMES_FEMALE <- c("Maria", "Anna", "Ursula", "Sandra", "Ruth", "Elisabeth", "Monika", "Claudia", "Verena", "Nicole", "Barbara", "Silvia", "Andrea", "Marie", "Daniela", "Christine", "Karin", "Marianne", "Erika", "Brigitte", "Margrit", "Laura", "Susanne", "Rita", "Sarah", "Katharina", "Esther", "Rosmarie", "Heidi", "Anita", "Manuela", "Doris", "Beatrice", "Sonja", "Rosa", "Yvonne", "Sara", "Jacqueline", "Gertrud", "Ana", "Irene", "Franziska", "Julia", "Cornelia", "Fabienne", "Gabriela", "Martina", "Eva", "Patricia", "Isabelle", "Sabrina", "Nathalie", "Edith", "Alexandra", "Corinne", "Melanie", "Angela", "Nadine", "Alice", "Elena", "Jessica", "Vanessa", "Denise", "Simone", "Anne", "Sophie", "Regula", "Nina", "Caroline", "Emma", "Susanna", "Carmen", "Tanja", "Lara", "Catherine", "Sabine", "Lea", "Petra", "Céline", "Jasmin", "Stefanie", "Therese", "Nadia", "Tamara", "Johanna", "Chantal", "Marina", "Michelle", "Christina", "Martha", "Monique", "Adelheid", "Rahel", "Dora", "Lisa", "Janine", "Hedwig", "Pia", "Anja", "Diana")
GENDER <- C("m", "f")

function createPerson(gender)