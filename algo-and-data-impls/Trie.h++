class Trie {

public:
  struct Node {
    using Arr = vector<unique_ptr<Node>>;
    variant<Arr, string> m;
    bool is_str() const { return holds_alternative<string>(m); }
    auto& str() { return get<string>(m); }
    auto& str() const { return get<string>(m); }
    auto& arr() { return get<Arr>(m); }
    auto& arr() const { return get<Arr>(m); }
    auto& arr(char c) { return arr().at(c == 0 ? 26 : c - 'a'); }
    auto& arr(char c) const { return arr().at(c == 0 ? 26 : c - 'a'); }
    void to_arr() { m = Arr(27); }
    template<typename It, typename S>
    bool eq(It it, S& s) const {
      auto s_end = s.data() + s.size() + 1;
      if (str().empty() && it == s_end) return true;
      return equal(str().data(), str().data() + str().size() + 1, it, s_end);
    }
  } root = {.m = Node::Arr(27)};

  /** Initialize your data structure here. */
  Trie() = default; 

  /** Inserts a word into the trie. */
  void insert(string word) {
    auto n = &root;
    auto it = word.data();
    while (true) {
      if (n->is_str()) {
        if (n->eq(it, word)) {
          break;
        } else {
          auto old_str = move(n->str());
          auto old_front = old_str.empty() ? 0 : old_str.front();
          if (old_front != 0) {
            old_str.erase(old_str.begin());
          }
          n->to_arr();
          n->arr(old_front) = make_unique<Node>(Node{.m = move(old_str)});
        }
      }
      auto& next = n->arr(*it);
      if (not next) {
        if (*it == 0) {
          word.erase();
        } else {
          word.erase(0, it - word.data() + 1);
        }
        next = make_unique<Node>(Node{.m = word});
        break;
      }
      n = next.get();
      ++it;
    }
  }

  /** Returns if the word is in the trie. */
  bool search(const string& word) const {
    auto n = &root;
    auto it = word.data();
    while (n) {
      if (n->is_str()) {
        return n->eq(it, word);
      }
      n = n->arr(*it).get();
      ++it;
    }
    return false;
  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  bool startsWith(const string& prefix) const {
    auto n = &root;
    auto it = prefix.data();
    while (n) {
      if (*it == 0) return true;
      if (n->is_str()) {
        auto [n_mm, p_mm] = std::mismatch(n->str().begin(), n->str().end(), it, prefix.data() + prefix.size());
        return *p_mm == 0;
      }
      n = n->arr(*it).get();
      ++it;
    }
    return false;
  }
};
