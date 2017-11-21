import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

import Type from './vue/damage_types/type.vue'

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin", {character: characterID}),
    damage_types: {}
  }
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
  components: { Type },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      headers: [
        {
          text: 'ID',
          align: 'left',
          value: 'id'
        },
        { text: 'Name', align: 'left', value: 'name' }
      ]
    }
  }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("damage_types", function(damage_types){
  store.state.damage_types = damage_types;
});
