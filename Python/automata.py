def read_transactions(lenTransactions, size):
    cubic_3d_matrix = [[[] for _ in range(size)] for _ in range(size)]
    for i in range(lenTransactions):
        transaction = list(str(input()).split(" "))
        cubic_3d_matrix[int(transaction[0]) - 1][int(transaction[2]) - 1].append(transaction[1])
    return cubic_3d_matrix


def ndfa_condition1(cubic_3d_matrix):
    line_content = []
    for line in range(len(cubic_3d_matrix)):
        for column in range(len(cubic_3d_matrix)):
            for char in cubic_3d_matrix[line][column]:
                if char in line_content:
                    return True
                else:
                    line_content.append(char)
        line_content.clear()
    return False


def ndfa_condition2(cubic_3d_matrix):
    for line in range(len(cubic_3d_matrix)):
        for column in range(len(cubic_3d_matrix)):
            if "_" in cubic_3d_matrix[line][column]:
                return True
    return False


def ndfa_or_dfa(cubic_3d_matrix, lenS0):
    condition_1 = ndfa_condition1(cubic_3d_matrix)
    condition_2 = ndfa_condition2(cubic_3d_matrix)
    condition_3 = True if lenS0 > 1 else False

    if condition_1 is True or condition_2 is True or condition_3 is True:
        print("NDFA")
    else:
        print("DFA")


def goal_exists_in_line(cubic_3d_matrix, line, goal):
    for count, column in enumerate(cubic_3d_matrix[line - 1]):
        if goal in cubic_3d_matrix[line - 1][count]:
            return True
    return False


def get_columns_of_goal(cubic_3d_matrix, line, goal):
    columns_with_goals = []
    for count, column in enumerate(cubic_3d_matrix[line - 1]):
        if goal in column:
            columns_with_goals.append(count+1)
    return columns_with_goals


def recognizes_word(all_paths, final):
    for path in all_paths:
        if int(path[-2]) in final:
            return path
    return "NO"


def find_path_rec(mtx_3D, line, goal, word_pos, current_word, word, path, all_paths):
    if current_word == word:
        columns_goal = get_columns_of_goal(mtx_3D, line, "_")
        if columns_goal:
            for col in columns_goal:
                find_path_rec(mtx_3D, col, goal, word_pos, current_word, word,
                              path + str(col) + " ", all_paths)
        else:
            all_paths.append(path)
    else:
        if goal_exists_in_line(mtx_3D, line, goal):
            columns_goal = get_columns_of_goal(mtx_3D, line, goal)
            for count in range(len(columns_goal)):
                new_word_char_pos = word_pos if (word_pos + 1 == len(word)) else word_pos + 1;
                find_path_rec(mtx_3D, columns_goal[count], word[new_word_char_pos], new_word_char_pos, current_word + word[word_pos], word, path + str(columns_goal[count]) + " ", all_paths)
        else:
            columns_goal = get_columns_of_goal(mtx_3D, line, "_")
            if columns_goal:
                for col in columns_goal:
                    find_path_rec(mtx_3D, col, goal, word_pos, current_word, word, path + str(col) + " ", all_paths)


# Main

# Read Input
cardS = int(input())
card_S0 = int(input())
set_S0 = list(map(int, str(input()).split(" ")))
card_F = int(input())
set_F = list(map(int, str(input()).split(" ")))
n_transitions = int(input())
mtx_3D = read_transactions(n_transitions, cardS)
word = str(input())

all_possible_paths = []
find_path_rec(mtx_3D, 1, word[0], 0, "", word, "1 ", all_possible_paths)

ndfa_or_dfa(mtx_3D, card_S0) # Output 1

recognize = recognizes_word(all_possible_paths, set_F)
if recognize == "NO":
    print("NO") # Output 2
else:
    print("YES") # Output 2
    print(recognize) # Output 3


