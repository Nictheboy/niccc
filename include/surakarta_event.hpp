// Copied from: https://github.com/surakarta-game/surakarta-core/blob/main/include/surakarta_event.h
// This is part of one of my previous labs.

#pragma once
#include <functional>

template <typename... Args>
class SurakartaEvent {
   public:
    void AddListener(std::function<void(Args...)> listener) {
        listeners_.push_back(listener);
    }

    void RemoveListeners() {
        listeners_.clear();
    }

    void Invoke(Args... args) {
        for (const auto& listener : listeners_) {
            listener(args...);
        }
    }

   private:
    std::vector<std::function<void(Args...)>> listeners_;
};