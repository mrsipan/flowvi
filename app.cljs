;; ──────────────────────────────────────────────
;; Vimflowy — Tree Outliner with Vim Keybindings
;; ──────────────────────────────────────────────

;; ── Utils ──
(defn gen-id [] (str "n" (random-uuid)))

;; ── Global State ──
(defonce app (atom nil))

(defn nodes [] (:nodes @app))
(defn focused [] (:focused @app))
(defn mode [] (:mode @app))
(defn help? [] (:help @app))

;; ── Persistence ──
(def storage-key "vimflowy-data-v2")

(defn save-local []
  (try
    (.setItem js/localStorage storage-key (js/JSON.stringify (clj->js @app)))
    (catch js/Error _ nil)))

(defn load-local []
  (try
    (let [raw (.getItem js/localStorage storage-key)]
      (when raw
        (let [parsed (js->clj (js/JSON.parse raw) :keywordize-keys true)]
          (reset! app parsed))
        true))
    (catch js/Error _ false)))

;; ── Tree Operations ──
(defn node-at [id] (get (nodes) id))
(defn parent-of [id] (:parent (node-at id)))
(defn children-of [id] (:children (node-at id)))
(defn depth [id]
  (loop [n id d 0]
    (let [p (parent-of n)]
      (if (or (nil? p) (= p "root")) d (recur p (inc d))))))

(defn flat-visible []
  (let [nds (nodes)]
    (loop [stack (list (get nds "root"))
           result []]
      (if (empty? stack)
        result
        (let [node (first stack)
              remaining (rest stack)]
          (if (:collapsed node)
            (recur remaining (conj result (:id node)))
            (recur (into (mapv (fn [id] (get nds id)) (reverse (:children node))) remaining)
                   (conj result (:id node)))))))))

(defn index-of [id]
  (let [vs (flat-visible)]
    (first (keep-indexed (fn [i v] (when (= v id) i)) vs))))

(defn node-at-index [idx]
  (let [vs (flat-visible)]
    (when (and (>= idx 0) (< idx (count vs)))
      (nth vs idx))))

(defn sibling-index [id]
  (let [p (parent-of id)]
    (when p
      (let [sibs (children-of p)]
        (first (keep-indexed (fn [i s] (when (= s id) i)) sibs))))))

(defn prev-sibling [id]
  (let [p (parent-of id) sibs (children-of p) si (sibling-index id)]
    (when (and si (> si 0)) (nth sibs (dec si)))))

(defn next-sibling [id]
  (let [p (parent-of id) sibs (children-of p) si (sibling-index id)]
    (when (and si (< si (dec (count sibs)))) (nth sibs (inc si)))))

;; ── Mutations ──
(defn update-node! [id f & args]
  (swap! app (fn [st]
               (let [nd (get-in st [:nodes id])]
                 (if nd
                   (assoc-in st [:nodes id] (apply f nd args))
                   st)))))

(defn set-text! [id text]
  (update-node! id assoc :text text))

(defn set-collapsed! [id v]
  (update-node! id assoc :collapsed v))

(defn toggle-collapsed! [id]
  (update-node! id update :collapsed not))

(defn toggle-completed! [id]
  (update-node! id update :completed not))

(defn focus! [id]
  (swap! app assoc :focused id))

(defn set-mode! [m]
  (swap! app assoc :mode m))

(defn toggle-help! []
  (swap! app update :help not))

(defn add-child! [parent-id]
  (let [nid (gen-id)]
    (swap! app (fn [st]
                 (-> st
                     (assoc-in [:nodes nid] {:id nid :text "" :parent parent-id :children [] :collapsed false :completed false})
                     (update-in [:nodes parent-id :children] #(vec (cons nid %))))))
    (focus! nid)
    nid))

(defn add-sibling-after! [id]
  (let [p (parent-of id)]
    (when p
      (let [nid (gen-id)
            sibs (children-of p)
            si (sibling-index id)]
        (swap! app (fn [st]
                     (-> st
                         (assoc-in [:nodes nid] {:id nid :text "" :parent p :children [] :collapsed false :completed false})
                         (update-in [:nodes p :children]
                                    (fn [chs]
                                      (let [before (subvec chs 0 (inc si))
                                            after (subvec chs (inc si))]
                                        (into (conj before nid) after)))))))
        (focus! nid)
        nid))))

(defn delete-node! [id]
  (when (not= id "root")
    (let [p (parent-of id)]
      (swap! app (fn [st]
                   (let [all-ids (fn rec [nid]
                                   (cons nid (mapcat rec (get-in st [:nodes nid :children]))))]
                     (-> st
                         (update :nodes #(apply dissoc % (all-ids id)))
                         (update-in [:nodes p :children] (fn [chs] (vec (remove #(= % id) chs))))))))
      (focus! (or (prev-sibling id) (parent-of id) "root")))))

(defn indent! [id]
  (let [ps (prev-sibling id)]
    (when (and ps (not= ps "root"))
      (let [p (parent-of id)
            sibs (children-of p)
            si (sibling-index id)]
        (swap! app (fn [st]
                     (-> st
                         (update-in [:nodes p :children] (fn [chs] (vec (remove #(= % id) chs))))
                         (update-in [:nodes ps :children] #(conj (vec %) id))
                         (assoc-in [:nodes id :parent] ps))))
        (focus! id)))))

(defn outdent! [id]
  (let [p (parent-of id)]
    (when (and p (not= p "root"))
      (let [gp (parent-of p)]
        (when gp
          (let [psibs (children-of p)
                si (sibling-index id)
                gp-sibs (children-of gp)
                pi (sibling-index p)]
            (swap! app (fn [st]
                         (-> st
                             (update-in [:nodes p :children] (fn [chs] (vec (remove #(= % id) chs))))
                             (update-in [:nodes gp :children]
                                        (fn [chs]
                                          (let [before (subvec chs 0 (inc pi))
                                                after (subvec chs (inc pi))]
                                            (into (conj before id) after))))
                             (assoc-in [:nodes id :parent] gp))))
            (focus! id)))))))

;; ── Navigation ──
(defn nav-down []
  (let [vs (flat-visible)
        f (focused)
        idx (index-of f)]
    (when idx
      (let [nxt (node-at-index (inc idx))]
        (when nxt (focus! nxt))))))

(defn nav-up []
  (let [vs (flat-visible)
        f (focused)
        idx (index-of f)]
    (when idx
      (let [prv (node-at-index (dec idx))]
        (when prv (focus! prv))))))

(defn nav-first []
  (let [vs (flat-visible)]
    (when (seq vs) (focus! (first vs)))))

(defn nav-last []
  (let [vs (flat-visible)]
    (when (seq vs) (focus! (last vs)))))

(defn collapse! [id]
  (when (seq (children-of id))
    (set-collapsed! id true)))

(defn expand! [id]
  (set-collapsed! id false))

;; ── DOM Rendering ──
(def app-el (.getElementById js/document "app"))

(defn create-style []
  (let [style (.createElement js/document "style")]
    (set! (.-textContent style)
          (str
           "* { box-sizing: border-box; margin: 0; padding: 0; }"
           "body { font-family: 'SF Mono', 'Fira Code', 'Consolas', monospace; background: #1a1a2e; color: #e0e0e0; font-size: 14px; overflow: hidden; height: 100vh; }"
           "#app { display: flex; flex-direction: column; height: 100vh; }"
           ".toolbar { display: flex; align-items: center; gap: 12px; padding: 8px 16px; background: #16213e; border-bottom: 1px solid #0f3460; flex-shrink: 0; }"
           ".toolbar button { background: #0f3460; color: #e0e0e0; border: 1px solid #1a5276; padding: 6px 12px; border-radius: 4px; cursor: pointer; font-family: inherit; font-size: 12px; }"
           ".toolbar button:hover { background: #1a5276; }"
           ".mode-indicator { font-weight: bold; padding: 4px 12px; border-radius: 4px; font-size: 12px; text-transform: uppercase; }"
           ".mode-normal { background: #0f3460; color: #64ffda; }"
           ".mode-insert { background: #1b5e20; color: #b9f6ca; }"
           ".tree-container { flex: 1; overflow-y: auto; padding: 8px 0; }"
           ".node-row { display: flex; align-items: center; cursor: pointer; padding: 2px 8px; border-left: 2px solid transparent; min-height: 28px; }"
           ".node-row:hover { background: rgba(255,255,255,0.03); }"
           ".node-row.focused { background: rgba(100,255,218,0.08); border-left-color: #64ffda; }"
           ".bullet { width: 16px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; color: #546e7a; font-size: 12px; user-select: none; }"
           ".bullet.expanded::before { content: '▾'; }"
           ".bullet.collapsed::before { content: '▸'; }"
           ".bullet.leaf::before { content: '•'; }"
           ".bullet.completed::before { content: '✓'; color: #4caf50; }"
           ".text-node { flex: 1; outline: none; white-space: pre-wrap; word-break: break-word; padding: 2px 4px; border-radius: 3px; }"
           ".text-node.completed { text-decoration: line-through; color: #666; }"
           ".text-node[contenteditable='true'] { background: rgba(255,255,255,0.05); cursor: text; }"
           ".status-bar { display: flex; align-items: center; gap: 16px; padding: 4px 16px; background: #16213e; border-top: 1px solid #0f3460; font-size: 11px; color: #888; flex-shrink: 0; }"
           ".help-overlay { position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.7); display: flex; align-items: center; justify-content: center; z-index: 1000; }"
           ".help-box { background: #16213e; border: 1px solid #0f3460; border-radius: 8px; padding: 24px; max-width: 500px; }"
           ".help-box h2 { color: #64ffda; margin-bottom: 16px; }"
           ".help-box kbd { background: #0f3460; padding: 2px 6px; border-radius: 3px; font-family: inherit; font-size: 12px; margin-right: 8px; }"
           ".help-box .help-row { margin-bottom: 4px; }"
           ".help-box .help-section { margin-bottom: 12px; }"
           ".help-box .help-section h3 { color: #aaa; font-size: 12px; margin-bottom: 4px; }"
           ))
    (.appendChild (.-head js/document) style)))

(defn node-depth [id]
  (loop [n id d 0]
    (let [p (parent-of n)]
      (if (or (nil? p) (= p "root")) d (recur p (inc d))))))

(defn render-node [id]
  (let [node (node-at id)
        d (node-depth id)
        f (focused)
        is-focused (= id f)
        is-collapsed (:collapsed node)
        has-children (seq (:children node))
        completed (:completed node)
        txt (:text node)
        row (.createElement js/document "div")]
    (.setAttribute row "data-id" id)
    (set! (.-className row) (str "node-row" (when is-focused " focused")))
    (set! (.. row -style -paddingLeft) (str (* d 20) "px"))
    
    ;; Bullet
    (let [bullet (.createElement js/document "span")]
      (set! (.-className bullet)
            (str "bullet "
                 (cond completed "completed"
                       (not has-children) "leaf"
                       is-collapsed "collapsed"
                       :else "expanded")))
      (.appendChild row bullet))
    
    ;; Text
    (let [text-el (.createElement js/document "span")]
      (set! (.-className text-el) (str "text-node" (when completed " completed")))
      (set! (.-textContent text-el) txt)
      (when (and is-focused (= (mode) :insert))
        (.setAttribute text-el "contenteditable" "true")
        (.addEventListener text-el "input" (fn [_]
                                             (set-text! id (.-textContent text-el))))
        (.addEventListener text-el "blur" (fn [_]
                                            (set-text! id (.-textContent text-el)))))
      (.appendChild row text-el))
    row))

(defn render-tree []
  (let [container (.createElement js/document "div")]
    (.setAttribute container "class" "tree-container")
    (let [vs (flat-visible)]
      (doseq [id vs]
        (when (not= id "root")
          (.appendChild container (render-node id)))))
    ;; Replace or append
    (let [existing (.querySelector js/document ".tree-container")]
      (when existing (.remove existing)))
    (.appendChild app-el container)))

(defn render-toolbar []
  (let [existing (.querySelector js/document ".toolbar")]
    (when existing (.remove existing)))
  (let [tb (.createElement js/document "div")]
    (.setAttribute tb "class" "toolbar")
    
    ;; Mode indicator
    (let [mi (.createElement js/document "span")]
      (set! (.-className mi) (str "mode-indicator " (if (= (mode) :insert) "mode-insert" "mode-normal")))
      (set! (.-textContent mi) (if (= (mode) :insert) "INSERT" "NORMAL"))
      (.appendChild tb mi))
    
    ;; Save button
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "💾 Save")
      (.addEventListener btn "click" (fn [_] (save-local)))
      (.appendChild tb btn))
    
    ;; Export button
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "📤 Export")
      (.addEventListener btn "click"
                         (fn [_]
                           (let [json-str (js/JSON.stringify (clj->js @app) nil 2)
                                 blob (js/Blob. #js[json-str] #js{:type "application/json"})
                                 url (.createObjectURL js/URL blob)
                                 a (.createElement js/document "a")]
                             (set! (.-href a) url)
                             (set! (.-download a) "vimflowy-export.json")
                             (.click a)
                             (.revokeObjectURL js/URL url))))
      (.appendChild tb btn))
    
    ;; Import button
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "📥 Import")
      (.addEventListener btn "click"
                         (fn [_]
                           (let [inp (.createElement js/document "input")]
                             (set! (.-type inp) "file")
                             (set! (.-accept inp) ".json")
                             (.addEventListener inp "change"
                                                (fn [e]
                                                  (let [file (aget (.. e -target -files) 0)]
                                                    (when file
                                                      (let [reader (js/FileReader.)]
                                                        (set! (.-onload reader)
                                                              (fn [ev]
                                                                (try
                                                                  (let [data (js->clj (js/JSON.parse (.. ev -target -result)) :keywordize-keys true)]
                                                                    (reset! app data)
                                                                    (render-all!)
                                                                    (save-local))
                                                                  (catch js/Error err
                                                                    (js/alert "Invalid JSON file")))))
                                                        (.readAsText reader))))))
                             (.click inp))))
      (.appendChild tb btn))
    
    ;; OneDrive buttons
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "☁️ OneDrive Save")
      (.addEventListener btn "click" one-drive-save)
      (.appendChild tb btn))
    
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "☁️ OneDrive Load")
      (.addEventListener btn "click" one-drive-load)
      (.appendChild tb btn))
    
    ;; Help
    (let [btn (.createElement js/document "button")]
      (set! (.-textContent btn) "? Help")
      (.addEventListener btn "click" (fn [_] (toggle-help!) (render-all!)))
      (.appendChild tb btn))
    
    (.appendChild app-el tb)))

(defn render-status-bar []
  (let [existing (.querySelector js/document ".status-bar")]
    (when existing (.remove existing)))
  (let [sb (.createElement js/document "div")]
    (set! (.-className sb) "status-bar")
    (let [nds (nodes)
          cnt (count (keys nds))]
      (set! (.-textContent sb) (str "Items: " (dec cnt) "  |  Mode: " (name (mode)) "  |  Focus: " (focused))))
    (.appendChild app-el sb)))

(defn render-help []
  (let [existing (.querySelector js/document ".help-overlay")]
    (when existing (.remove existing)))
  (when (help?)
    (let [overlay (.createElement js/document "div")]
      (.setAttribute overlay "class" "help-overlay")
      (.addEventListener overlay "click" (fn [e]
                                           (when (= (.-target e) overlay)
                                             (toggle-help!)
                                             (render-all!))))
      (let [box (.createElement js/document "div")]
        (.setAttribute box "class" "help-box")
        (set! (.-innerHTML box)
              (str
               "<h2>Vimflowy Keybindings</h2>"
               "<div class='help-section'><h3>Movement (NORMAL mode)</h3>"
               "<div class='help-row'><kbd>j</kbd> Down</div>"
               "<div class='help-row'><kbd>k</kbd> Up</div>"
               "<div class='help-row'><kbd>gg</kbd> Go to first</div>"
               "<div class='help-row'><kbd>G</kbd> Go to last</div>"
               "</div>"
               "<div class='help-section'><h3>Editing (NORMAL mode)</h3>"
               "<div class='help-row'><kbd>i</kbd> Enter insert mode</div>"
               "<div class='help-row'><kbd>o</kbd> Add child node</div>"
               "<div class='help-row'><kbd>O</kbd> Add sibling above</div>"
               "<div class='help-row'><kbd>dd</kbd> Delete node</div>"
               "<div class='help-row'><kbd>x</kbd> Toggle complete</div>"
               "</div>"
               "<div class='help-section'><h3>Structure</h3>"
               "<div class='help-row'><kbd>h</kbd> Collapse</div>"
               "<div class='help-row'><kbd>l</kbd> Expand</div>"
               "<div class='help-row'><kbd>&gt;</kbd> Indent</div>"
               "<div class='help-row'><kbd>&lt;</kbd> Outdent</div>"
               "</div>"
               "<div class='help-section'><h3>Other</h3>"
               "<div class='help-row'><kbd>Esc</kbd> Normal mode / close</div>"
               "<div class='help-row'><kbd>?</kbd> Toggle help</div>"
               "</div>"))
        (.appendChild overlay box))
      (.appendChild app-el overlay))))

(defn render-all! []
  (render-toolbar)
  (render-tree)
  (render-status-bar)
  (render-help))

;; ── Keyboard Handler ──
(defonce key-seq (atom []))
(defonce key-timer (atom nil))

(defn handle-key [e]
  (let [key (.-key e)
        ctrl (.-ctrlKey e)
        m (mode)]
    
    ;; Global: Escape
    (when (= key "Escape")
      (.preventDefault e)
      (when (help?) (toggle-help!) (render-all!))
      (when (= m :insert)
        ;; Save current text before exiting insert mode
        (let [active-el (.-activeElement js/document)]
          (when (and active-el (.. active-el -classList (contains "text-node")))
            (set-text! (focused) (.-textContent active-el))))
        (set-mode! :normal)
        (render-all!))
      (js/clearTimeout @key-timer)
      (reset! key-seq [])
      (when (not= m :normal) (render-all!)))
    
    ;; Global: ? toggles help
    (when (and (= key "?") (not ctrl) (= m :normal) (not (help?)))
      (.preventDefault e)
      (toggle-help!)
      (render-all!))
    
    ;; Normal mode keys
    (when (= m :normal)
      (let [seq-str (str/join "" (conj @key-seq key))]
        (cond
          (= key "j") (do (.preventDefault e) (nav-down) (render-tree) (render-status-bar) (reset! key-seq []))
          
          (= key "k") (do (.preventDefault e) (nav-up) (render-tree) (render-status-bar) (reset! key-seq []))
          
          (= key "i") (do (.preventDefault e) (set-mode! :insert) (render-all!))
          
          (= key "o") (do (.preventDefault e)
                          (let [f (focused)
                                nid (add-child! f)]
                            (set-mode! :insert)
                            (render-all!)))
          
          (= key "O") (do (.preventDefault e)
                          (let [f (focused)]
                            (when (not= f "root")
                              (let [nid (add-sibling-after! f)]
                                (set-mode! :insert)
                                (render-all!)))))
          
          (= key "h") (do (.preventDefault e) (collapse! (focused)) (render-tree) (render-status-bar))
          
          (= key "l") (do (.preventDefault e) (expand! (focused)) (render-tree) (render-status-bar))
          
          (= key "x") (do (.preventDefault e) (toggle-completed! (focused)) (render-tree) (render-status-bar))
          
          (= key ">") (do (.preventDefault e) (indent! (focused)) (render-tree) (render-status-bar))
          
          (= key "<") (do (.preventDefault e) (outdent! (focused)) (render-tree) (render-status-bar))
          
          (= key "G") (do (.preventDefault e) (nav-last) (render-tree) (render-status-bar) (reset! key-seq []))
          
          (= seq-str "gg") (do (.preventDefault e) (nav-first) (render-tree) (render-status-bar) (reset! key-seq []))
          
          (= seq-str "dd") (do (.preventDefault e) (delete-node! (focused)) (render-all!) (reset! key-seq []))
          
          ;; Otherwise track for multi-key sequences
          (not (contains? #{"Shift" "Control" "Alt" "Meta" "Tab"} key))
          (do
            (swap! key-seq conj key)
            (js/clearTimeout @key-timer)
            (reset! key-timer (js/setTimeout (fn [] (reset! key-seq [])) 500))))))
    
    ;; Insert mode: Enter adds sibling
    (when (and (= m :insert) (= key "Enter") (not ctrl))
      (.preventDefault e)
      ;; Save current text before adding sibling
      (let [active-el (.-activeElement js/document)]
        (when (and active-el (.. active-el -classList (contains "text-node")))
          (set-text! (focused) (.-textContent active-el))))
      (let [f (focused)]
        (add-sibling-after! f)
        (render-all!)))))

;; ── Click Handler ──
(defn handle-click [e]
  (let [target (.-target e)
        row (.closest target ".node-row")]
    (when row
      (let [id (.getAttribute row "data-id")]
        (when id
          (focus! id)
          ;; If clicking the text area, enter insert mode
          (when (or (.matches target ".text-node")
                    (.matches target ".node-row"))
            (set-mode! :insert)
            (render-all!))
          (when (not (or (.matches target ".text-node")
                         (.matches target ".node-row")))
            (render-all!)))))))

;; ── OneDrive Integration ──
(def one-drive-client-id "00000000-0000-0000-0000-000000000000") ;; placeholder
(def one-drive-token (atom nil))

(defn one-drive-auth []
  (js/alert "OneDrive: Configure your client-id in the source (one-drive-client-id) and set redirect URI to this page."))

(defn one-drive-save [_]
  (if @one-drive-token
    (let [content (js/JSON.stringify (clj->js @app))
          filename "vimflowy-data.json"]
      (-> (js/fetch (str "https://graph.microsoft.com/v1.0/me/drive/root:/" filename ":/content")
                    #js{:method "PUT"
                        :headers #js{"Authorization" (str "Bearer " @one-drive-token)
                                     "Content-Type" "application/json"}
                        :body content})
          (.then (fn [r] (if (.-ok r) (js/alert "Saved to OneDrive!") (js/alert "Save failed. Token may be expired."))))
          (.catch (fn [_] (js/alert "Network error")))))
    (one-drive-auth)))

(defn one-drive-load [_]
  (if @one-drive-token
    (-> (js/fetch "https://graph.microsoft.com/v1.0/me/drive/root:/vimflowy-data.json:/content"
                  #js{:headers #js{"Authorization" (str "Bearer " @one-drive-token)}})
        (.then (fn [r] (.json r)))
        (.then (fn [data]
                 (let [d (js->clj data :keywordize-keys true)]
                   (reset! app d)
                   (render-all!)
                   (save-local)
                   (js/alert "Loaded from OneDrive!"))))
        (.catch (fn [_] (js/alert "Load failed. Token may be expired."))))
    (one-drive-auth)))

;; ── Auto-save ──
(defonce auto-save-interval
  (js/setInterval
   (fn []
     (when (and @app (not= (:mode @app) :insert))
       (save-local)))
   5000))

;; ── Init ──
(defn init! []
  (create-style)
  ;; Try loading from localStorage
  (when-not (load-local)
    ;; Fresh state
    (let [root-id "root"
          c1 (gen-id)
          c2 (gen-id)
          gc (gen-id)]
      (reset! app {:nodes {root-id {:id root-id :text "Vimflowy" :parent nil :children [c1 c2] :collapsed false :completed false}
                           c1 {:id c1 :text "Welcome to Vimflowy!" :parent root-id :children [gc] :collapsed false :completed false}
                           c2 {:id c2 :text "Press ? for help" :parent root-id :children [] :collapsed false :completed false}
                           gc {:id gc :text "Try j/k to navigate, o to add" :parent c1 :children [] :collapsed false :completed false}}
                   :focused root-id
                   :mode :normal
                   :help false})))
  ;; Event listeners
  (.addEventListener js/document "keydown" handle-key)
  (.addEventListener js/document "click" handle-click)
  (render-all!))

(init!)
