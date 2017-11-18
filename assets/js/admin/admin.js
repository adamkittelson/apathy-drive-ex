import {Socket} from "phoenix"
import ShoppingList from './vue/shopping_list.vue'
import Type from './vue/damage_types/type.vue'

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin", {character: characterID}),
    damage_types: {}
  }
})

window.shopping_list = new Vue({
  el: '#shopping-list',
  template: '<ShoppingList/>',
  components: { ShoppingList }
})

window.damage_types = new Vue({
  el: "#app",
  store,
  template: '#damage-types',
  computed: {
    sorted_damage_types: function() {
      var list = Object.values(this.$store.state.damage_types)
      return _.sortBy(list, ["id"])
    }
  },
  components: { Type }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("damage_types", function(damage_types){
  store.state.damage_types = damage_types;
});

