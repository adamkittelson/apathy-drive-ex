import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin:abilities", {character: characterID}),
    abilities: {},
    sortBy: "id",
    descending: false,
    page: 1,
    valid_targets: [],
    kinds: []
  },
  mutations: {
    update_ability(state, form_data) {
      console.log("updating " + form_data.name)
      var index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.id
      })
      state.abilities[index].data = Object.assign({}, form_data)
      state.channel.push("update", form_data)
      .receive("ok", function() {
        console.log(form_data.name + " Updated")
        state.abilities[index].dialog = false
      })
    },
    setAbilities(state, abilities) {
      state.abilities = abilities;
    },
    updatePagination(state, payload) {
      state.sortBy = payload.sortBy
      state.descending = payload.descending
      state.page = payload.page
    }
  }
})

window.abilities = new Vue({
  el: "#app",
  store,
  template: '#abilities',
  computed: {
    abilities: function() {
      var abilities = Object.values(this.$store.state.abilities)
      return abilities
    }
  },
  methods: {
    updatePageNumber: function(page) {
      this.$store.commit('updatePagination', {page: page, sortBy: this.$store.state.sortBy, descending: this.$store.state.descending})
      this.fetchPage()
    },
    updatePagination: function(pagination) {
      this.$store.commit('updatePagination', {page: this.$store.state.page, sortBy: pagination.sortBy, descending: !!pagination.descending})
      this.fetchPage()
    },
    fetchPage: function() {
      if (this.$store.state.channel.state === "joined") {
        abilities.loading = true
        var args = {query: abilities.search, page_number: this.$store.state.page, order_by: this.$store.state.sortBy, descending: this.$store.state.descending}
        this.$store.state.channel.push("fetch_page", args)
      }
    },
    submit(form_data) {
      if (this.$refs["form-" + form_data.id].validate()) {
        // Native form submission is not yet supported
        this.$store.commit('update_ability', form_data)
      }
    },
    valid_targets: function() {
      return this.$store.state.valid_targets
    },
    kinds: function() {
      return this.$store.state.kinds
    }
  },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      nameRules: [
        (v) => !!v || 'Name is required',
        (v) => v && v.length <= 30 || 'Name must be less than 30 characters'
      ],
      search: '',
      loading: true,
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

store.state.channel.on("abilities", function(data){
  abilities.loading = false
  var items = _.map(data.page.entries, function(ability) {
    var item = {
      form: Object.assign({}, ability),
      dialog: false,
      valid: true,
      data: Object.assign({}, ability)
    }
    return item;
  })
  store.commit('setAbilities', items)
  store.state.valid_targets = data.valid_targets;
  store.state.kinds = data.kinds;
  abilities.pagination = {
    page: data.page.page_number,
    rowsPerPage: data.page.page_size,
    totalItems: data.page.total_entries,
    totalPages: data.page.total_pages
  }
});
