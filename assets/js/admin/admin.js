import {Socket} from "phoenix"
import ShoppingList from './vue/shopping_list.vue'
import Type from './vue/damage_types/type.vue'

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

const store = new Vuex.Store({
  state: {
    channel: socket.channel("admin", {character: characterID})
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
  data: {
    damage_types: {}
  },
  computed: {
    sorted_damage_types: function() {
      var list = Object.values(this.damage_types)
      console.log(list)
      return _.sortBy(list, ["id"])
    }
  },
  components: { Type }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("damage_types", function(data){
  console.log(data.damage_types);
  damage_types.damage_types = data.damage_types;
});

