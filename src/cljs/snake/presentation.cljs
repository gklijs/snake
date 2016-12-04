(ns snake.presentation
    (:require [re-frame.core :refer [subscribe]]))

(def my-presentation
    [
    {
        :title "A little about me"
        :first "Been developing some years with Hippo doing back-end stuff. One year of working without Hippo, building a Kafka centered environment at Rabobank to share business events. Recently grew an interest in Clojure and Clojurescript."
        :second "Originally from Roosendaal, lived in Leiden and Utrecht, and moving to Papendrecht next month, to live with Matha her kids, and her cats."
        :third "Now working at UvA/HvA, getting to hippo 10."
    }
    {
        :title "Start of devoxx"
        :first "Has some fun parts of talking to robots, and then waiting till they get the answer from some other computer"
        :second "Also some nice low-level stuff about compilers at the end."
        :youtube "T-ujPlfm6p4"
    }
    {
        :title "Venkat - Make code suck less"
        :first "Nice to listen to, we really need slack time."
        :second "A good code is like a good joke."
        :third "Reduce state, one of the reasons to go to clojure"
        :youtube "nVZE53IYi4w"
    }
    {
        :title "Osinski - Ai philosophy"
        :first "Very theoretical, but some nice points."
        :second "What when we don't know anymore why a computer does the things it does?"
        :youtube "XD3DAE_eWJ8"
    }
    {
        :title "Robinson and Atamel - Machine learning api's."
        :first "Easy ways to let pictures or speech interpreted by trained machines."
        :youtube "kiy1uTW1CRI"
    }
    {
        :title "Vitz - Introduction to clojure"
        :first "Also worth watching if you have no knowledge of Closure yet."
        :second "No necessity for Clojurescript to generate html."
        :youtube "3Xm_nVqxowk"
    }
    {
        :title "Gupta - Minecraft representation of docker"
        :first "Nice way for children to start exploring docker."
        :youtube "b3OWQ7mO6Ec"
    }
    {
        :title "Yanaga - Vert.x introduction"
        :first "Event driven and non blocking tool-kit."
        :second "Lots of languages and functions supported."
        :youtube "7IbdWcdlYOI"
    }
    {
        :title "Maurer - Netty, like Vert.x"
        :first "Widely used, for example in Kafka."
        :second "Also about combining C with java, and memory management."
        :youtube "DKJ0w30M0vg"
    }
    {
        :title "Clojurescript"
        :first "Started with a snake game in clojure from github. last week it really works"
        :second "Updates the versions, made it responsive, turned it into a Luminus project to add a back-end."
        :third "Would like to add some ai snakes and train them."
        :youtube "wq6ctyZBb0A"
    }
    {
        :title "Experience with Clojurescript"
        :first "Feedback from figwheel is very helpful, also reloads the css."
        :second "Only one tool is needed for both front and back-end."
        :third "Doesn't play well but Spring, since it's anti-framework."
        :youtube "SLRSOyR47Ro"
    }
    {
        :title "Maybe some questions/demo/code?"
        :first "If time allows it."
    }
    ])

;; -- View Components ---------------------------------------------------------

(defn render-slide
    "will show different content, depending on the number of the slide"
    []
    (let [slide (subscribe [:slide])]
    (fn
        []
        (cond
            (> 0 @slide)[:div.slide [:h1 "Myself, Devoxx, Clojure, Clojurescript"][:p][:img {:src "/img/clojure-logo.png"}][:p][:p "Press right to go to the first slide"]]
            (<= (count my-presentation) @slide)[:div.slide [:h1 "The end"][:p][:img {:src "/img/end.png"}][:p][:p "Press left to go back to the slide"]]
            :default (let [my-slide (nth my-presentation @slide)]
                [:div.slide
                [:h1.presentation-title (:title my-slide)]
                (if (:first my-slide) [:p.presentation-text (:first my-slide)])
                (if (:second my-slide) [:p.presentation-text (:second my-slide)])
                (if (:third my-slide) [:p.presentation-text (:third my-slide)])
                (if (:youtube my-slide) [:iframe {:width "100%" :height "700px" :src (str "https://www.youtube.com/embed/" (:youtube my-slide))}])
            ])
        )
    )))

(defn presentation
  "The presentation rendering function"
  []
  [:div
   [:div.container [:div.row.flex-items-xs-center
       [:div.col-xs [render-slide]]]
    ]])